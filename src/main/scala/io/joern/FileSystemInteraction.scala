package io.joern

import java.io.File
import scala.annotation.tailrec
import scala.io.StdIn.readLine

class FileAlreadyExists(path: String) extends Error {

  override def getMessage: String =
    s"the file ${path} already exists and shall not be overwritten"

}

object FileSystemInteraction {

  def fileExists(path: String): Boolean = {
    new File(path).exists()
  }

  @tailrec
  def askPermission(message: String): Boolean = {
    print(message + " [y/N]")
    readLine() match {
      case "y" | "Y" => true
      case "n" | "N" => false
      case _         => askPermission(message)
    }
  }

  def interactiveOverwriteCheck(path: String): String = {
    if (!fileExists(path) || askPermission(s"$path already exists. Overwrite?")) {
      path
    } else {
      throw new FileAlreadyExists(path)
    }
  }

  def deleteDirectory(dir: File): Unit = {
    // if the provided file is a directory
    if (dir.isDirectory) {
      // get the children of the directory
      val children: Array[File] = dir.listFiles()
      // iterate over all children
      children.foreach { child =>
        // if a child is a directory BUT NOT A SYMLINK
        if (child.isDirectory && !java.nio.file.Files.isSymbolicLink(
              child.toPath)) {
          // recurse
          deleteDirectory(child)
        } else {
          // only delete the symlink but do not recurse
          child.delete()
        }
        // if a child is a proper file
        if (child.isFile) {
          // delete
          child.delete()
        }
      }
      // now also delete
      dir.delete()
    } else {
      // well we expected a directory didn't we?
      throw new RuntimeException(s"${dir.getAbsolutePath} is not a directory")
    }
  }

}
