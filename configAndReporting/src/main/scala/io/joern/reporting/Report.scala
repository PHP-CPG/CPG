package io.joern.reporting

import io.joern.reporting.JSONConverter._
import spray.json._

import java.io.{File, FileWriter}
import scala.collection.mutable.ListBuffer

class Report(val passReports: ListBuffer[PassReport] = ListBuffer(),
             var linkingReport: Option[LinkingReport]) {

  def success: Boolean = !passReports.exists(_.errors.nonEmpty)

  /** tests whether the cpg creation was successfull within specs
    *
    * this test allows for parsing issues as well as {closure} errors in
    *
    * @return
    */
  def successWithinSpecs: Boolean = {
    passReports
      .filter(_.name != "FileParser") // parsing errors are expected in large projects
      .flatMap(_.errors)
      .isEmpty
  }

  def addReport(report: PassReport): Unit = {
    passReports.synchronized {
      passReports.addOne(report)
    }
  }

  def addReport(report: LinkingReport): Unit = {
    linkingReport = Some(report)
  }

  def writeToFile(file: String): Unit = {
    val fileWriter = new FileWriter(new File(file))
    try {
      fileWriter.write(this.toJson.prettyPrint)
    } finally {
      fileWriter.flush()
      fileWriter.close()
    }
  }

  def prettyPrintErrors: String = {
    val sb = new StringBuilder()
    passReports.filter(_.errors.nonEmpty).foreach { report =>
      sb.append(report.name + "\n")
      report.errors.foreach { err =>
        sb.append(err.toString + "\n")
      }
    }
    sb.toString()
  }

}

object Report {

  def apply(): Report = {
    new Report(ListBuffer(), None)
  }

  def apply(file: String): Report = {
    val source = scala.io.Source.fromFile(file)
    val lines = try source.mkString
    finally source.close()
    spray.json.JsonParser(lines).convertTo[Report]
  }

}
