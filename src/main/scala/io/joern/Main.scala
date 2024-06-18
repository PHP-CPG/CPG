package io.joern

import io.joern.bytecode.parser.PHPVersion
import io.joern.bytecode.util.FilterFiles.filterFiles
import io.joern.config.CPGConfig
import io.joern.reporting.Report
import io.shiftleft.x2cpg.SourceFiles

import java.io.File
import java.nio.file.Files
import scala.sys.exit

object Main {

  def main(args: Array[String]): Unit = {
    val config = Option(io.joern.config.CPGConfig.initializeConfig(args))
      .getOrElse(return )
    if (!config.forced) {
      FileSystemInteraction.interactiveOverwriteCheck(config.output)
    }
    val layerReport: Option[Report] = try {
      config.layer.get match {
        case "bytecode"   => Some(createByteCodeCPG(config))
        case "sourcecode" => throw new NotImplementedError()
        case "all"        => throw new NotImplementedError()
        case "log-profiler" =>
          None
      }
    } catch {
      case x: Throwable =>
        throw new RuntimeException(
          s"We encountered a unmitigated error. Please create a bug report for $x: ${x.getMessage}.")
    }
    layerReport match {
      case Some(layerReport) =>
        if (config.report) {
          val reportOut = config.output + ".report"
          layerReport.writeToFile(reportOut)
        }
        if (layerReport.successWithinSpecs) {
          exit(0)
        } else {
          exit(1)
        }
      case None =>
    }
  }

  private def createByteCodeCPG(config: CPGConfig): Report = {
    val newRootFolder = config.rootFolder
    val sourceFilesNames = SourceFiles
      .determine(
        Set(newRootFolder),
        config.files.toSet
      )
      .distinct
    val sourceFiles = filterFiles(sourceFilesNames.map(new File(_)))
    implicit val version: PHPVersion.Value = config.phpversion match {
      case "7" => PHPVersion.V7
      case "8" => PHPVersion.V8
      case x   => throw new RuntimeException(s"PHP version $x is not supported")
    }
    val byteCodeCpg = new bytecode.PhpToCpg()
    var report: Option[Report] = None
    try {
      byteCodeCpg
        .createCpg(sourceFiles, config.output, config)
        .close()
    } finally {
      report = Some(byteCodeCpg.getFinalReport)
    }
    report.getOrElse(
      throw new RuntimeException(
        "there is no report! Really bad sign - good luck debugging"))
  }

}
