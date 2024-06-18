package io.joern.bytecode.util

import java.io.File
import java.nio.file.Files

object FilterFiles {
  def filterFiles(files: List[File]): List[File] =
    files.filter(_.isFile).filterNot(x => Files.isSymbolicLink(x.toPath))
}
