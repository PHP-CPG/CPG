package io.joern.php

import better.files._
import io.joern.php.passes._
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.IntervalKeyPool
import io.shiftleft.x2cpg.SourceFiles
import overflowdb.Node

import scala.jdk.CollectionConverters._

case class CpgTestFixture(projectName: String)
    extends io.joern.AbstractCpgTestFixture {
  import CpgTestFixture._

  private val dirName: String =
    String.format("layerSourceCode/resources/unittesting/testprojects/%s",
                  projectName)
  val filenames: List[String] = SourceFiles.determine(Set(dirName), Set(".php"))
  override var cpg: Cpg = cpgForDir(filenames)

  def V: Iterator[Node] = cpg.graph.V.asScala

}

case class CpgFromCodeTestFixture(code: String)
    extends io.joern.AbstractCpgTestFixture {
  import CpgTestFixture._

  override var cpg: Cpg = _
  File.usingTemporaryDirectory("php2cpg") { dir =>
    (dir / "test.php").write("<?php " + code + " ?>")
    val dirname = dir.path.toAbsolutePath.toString
    val filenames: List[String] =
      SourceFiles.determine(Set(dirname), Set(".php"))
    cpg = cpgForDir(filenames)
  }
}

object CpgTestFixture {

  def cpgForDir(filenames: List[String]): Cpg = {
    val cpg: Cpg = Cpg.emptyCpg
    val keyPool = new IntervalKeyPool(1001, 2000)
    new AstCreationPass(filenames, cpg, keyPool).createAndApply()
    cpg
  }

}
