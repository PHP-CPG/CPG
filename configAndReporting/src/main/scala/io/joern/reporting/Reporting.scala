package io.joern.reporting

import io.shiftleft.codepropertygraph.generated.nodes.{CfgNode, Method}
import wvlet.log.LogSupport

import java.io.{OutputStream, PrintStream}
import java.time.LocalDateTime
import scala.collection.mutable.ListBuffer

case class ReportableError(file: String,
                           line: Int,
                           method: String,
                           bytecode: String,
                           description: String)
    extends Throwable {}

object ReportableError {

  def apply(method: Method,
            cfgNode: CfgNode,
            description: String): ReportableError = {
    val lineNumber: Int =
      cfgNode.lineNumber.getOrElse(Integer.getInteger("-1")).toInt
    ReportableError(method.filename,
                    lineNumber,
                    method.fullName,
                    cfgNode.code,
                    description)
  }

}

object Reporting {

  def stringifyErrorStack(x: Throwable): String = {
    val str: StringBuilder = new StringBuilder()
    val printTo = new PrintStream(new OutputStream {
      override def write(b: Int): Unit = {
        str.addOne(b.toChar)
      }
      override def toString: String = str.toString()
    })
    x.printStackTrace(printTo)
    str.toString()
  }

}

trait Reporting extends LogSupport {

  val name: String
  val start: LocalDateTime = LocalDateTime.now()

  val elb: ListBuffer[PassError] = new ListBuffer[PassError]()
  val wlb: ListBuffer[PassWarning] = new ListBuffer[PassWarning]()

  private def withErrorReporting(method: Option[Method])(
      func: => Unit): Unit = {
    try {
      func
    } catch {
      case err: ReportableError => reportError(err)
      case thr: Throwable =>
        method match {
          case Some(value) =>
            reportError(value.filename,
                        "NA",
                        value.fullName,
                        "NA",
                        thr.getMessage,
                        thr)
          case None =>
            reportError("NA", "NA", "NA", "NA", thr.getMessage, thr)
        }
    }
  }

  protected def withErrorReporting(method: Method)(func: => Unit): Unit = {
    withErrorReporting(Some(method))(func)
  }

  protected def withErrorReporting()(func: => Unit): Unit = {
    withErrorReporting(None)(func)
  }

  protected def reportWarning(file: String,
                              line: String,
                              method: String,
                              bytecode: String,
                              description: String): Unit = {
    wlb.synchronized {
      val warning = PassWarning(file, line, method, bytecode, description)
      warn(warning)
      wlb.addOne(warning)
    }
  }

  protected def reportWarning(method: Method,
                              cfgNode: CfgNode,
                              description: String): Unit = {
    reportWarning(
      method.filename,
      cfgNode.lineNumber.getOrElse(Integer.getInteger("-1")).toString,
      method.fullName,
      cfgNode.code,
      description)
  }

  @deprecated(
    "Pass the underlying throwable as well as last argument! Otherwise debugging is hard!")
  protected def reportError(file: String,
                            line: String,
                            method: String,
                            bytecode: String,
                            description: String): Unit = {
    reportError(file, line, method, bytecode, description, None)
  }

  protected def reportError(file: String,
                            line: String,
                            method: String,
                            bytecode: String,
                            description: String,
                            e: Throwable): Unit = {
    reportError(file, line, method, bytecode, description, Some(e))
  }

  private def reportError(file: String,
                          line: String,
                          method: String,
                          bytecode: String,
                          description: String,
                          e: Option[Throwable]): Unit = {
    elb.synchronized {
      val err = PassError(file, line, method, bytecode, description, e match {
        case Some(value) => Reporting.stringifyErrorStack(value)
        case None        => "no error provided"
      })
      error(err)
      elb.addOne(err)
    }
  }

  protected def reportError(err: ReportableError): Unit = {
    reportError(err.file,
                err.line.toString,
                err.method,
                err.bytecode,
                err.description,
                Some(err))
  }

  def getReport: PassReport = {
    PassReport(name, elb.toList, wlb.toList, start)
  }

}
