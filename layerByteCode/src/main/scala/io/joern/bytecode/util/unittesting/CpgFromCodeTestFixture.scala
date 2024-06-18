package io.joern.bytecode.util.unittesting

import better.files.File
import io.joern.bytecode.PhpToCpg
import io.joern.bytecode.parser.PHPVersion.PHPVersion
import io.joern.bytecode.util.FilterFiles.filterFiles
import io.joern.bytecode.util.unittesting.CpgFromCodeTestFixture.{
  cpgForDir,
  setTimestamp
}
import io.joern.config.CPGConfig
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.x2cpg.SourceFiles
import overflowdb.Node

import java.io.{File => JFile}
import java.nio.charset.StandardCharsets
import java.text.SimpleDateFormat
import scala.jdk.CollectionConverters._

object CpgFromCodeTestFixture {
  def cpgForDir(files: List[JFile], configFile: Option[String] = None)(
      implicit version: PHPVersion): Cpg = {
    val cpg: Cpg = Cpg.emptyCpg
    val config = configFile match {
      case Some(value) => CPGConfig.initializeConfig(value)
      case None        => CPGConfig.initializeConfig()
    }
    val cpgCreator = new PhpToCpg()
    try {
      cpgCreator.populateCpg(files, cpg, config)
    } finally {
      val finalReport = cpgCreator.getFinalReport
      if (!finalReport.success) {
        throw new RuntimeException(
          s"Creation of CPG was not successful with fromCodeFixture \n ${finalReport.prettyPrintErrors}")
      }
    }
  }

  // Workaround for a bug in PHP:
  // we set the timestamp to a date way in the past here
  // because there seems to be a bug in `php` which causes
  // the error stream to be empty for freshly created files.
  def setTimestamp(filePath: String): Boolean = {
    val file = new JFile(filePath)
    val date = new SimpleDateFormat("MM/dd/yyyy")
    val last = date.parse("10/03/1990")
    file.setLastModified(last.getTime)
  }
}

case class CpgFromCodeTestFixture(
    code: String,
    insertPhpTags: Boolean = true,
    configFile: Option[String] = None)(implicit val version: PHPVersion)
    extends AbstractCpgTestFixture {
  override implicit var cpg: Cpg = _

  File.usingTemporaryDirectory("php2cpg") { tmpFolder =>
    val filePath = s"$tmpFolder/test.php"
    if (insertPhpTags) {
      File(filePath).write(s"""<?php
                              |$code
                              |?>""".stripMargin)(
        charset = StandardCharsets.UTF_8)
    } else {
      File(filePath).write(code)(charset = StandardCharsets.UTF_8)
    }
    setTimestamp(filePath)
    val files: List[JFile] = filterFiles(
      SourceFiles
        .determine(Set(tmpFolder.path.toString), Set(".php"))
        .distinct
        .map(x => new JFile(x)))
    cpg = cpgForDir(files, configFile)
  }

  def V: Iterator[Node] = cpg.graph.V.asScala

}
