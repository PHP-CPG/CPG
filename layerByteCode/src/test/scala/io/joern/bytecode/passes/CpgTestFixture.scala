package io.joern.bytecode.passes

import io.joern.bytecode.PhpToCpg
import io.joern.bytecode.parser.PHPVersion.PHPVersion
import io.joern.bytecode.util.FilterFiles.filterFiles
import io.joern.bytecode.util.unittesting.AbstractCpgTestFixture
import io.joern.config.CPGConfig
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.x2cpg.SourceFiles
import overflowdb.Node

import java.io.{File => JFile}
import java.text.SimpleDateFormat
import scala.jdk.CollectionConverters._

case class CpgTestFixture(projectName: String)(implicit version : PHPVersion)  extends AbstractCpgTestFixture {
  import CpgTestFixture._

  private val dirName: String =
    String.format("layerByteCode/resources/unittesting/testprojects/%s",
                  projectName)
  val files: List[JFile] = filterFiles(SourceFiles.determine(Set(dirName), Set(".php")).distinct.map(new JFile(_)))
  override implicit var cpg: Cpg = cpgForDir(files)

  def V: Iterator[Node] = cpg.graph.V.asScala
}


object CpgTestFixture {

  def cpgForDir(files: List[JFile])(implicit version : PHPVersion) : Cpg = {
    val cpg: Cpg = Cpg.emptyCpg
    val config = CPGConfig.initializeConfig()
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
