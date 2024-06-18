package io.joern.reporting

import de.halcony.argparse.{Parser, ParsingResult}

import java.io.File
import java.time.temporal.ChronoUnit
import scala.collection.mutable.{ListBuffer, Map => MMap}
import scala.util.{Failure, Success, Try}

object LogEvaluation {

  val parser: Parser = Parser("log-evaluation")
    .addSubparser(
      Parser("profile")
        .addDefault[String]("layer", "log-profiler"))

  def profiler(pargs: ParsingResult): Unit = {
    val folder = pargs.getValue[String]("rootFolder")
    if (!new File(folder).isDirectory) {
      throw new RuntimeException("You need to provide a proper folder")
    } else {
      val deltas: MMap[String, ListBuffer[Long]] = MMap()
      new File(folder)
        .listFiles()
        //.map{elem => println(elem.getAbsolutePath); elem}
        .filter(_.isFile)
        //.map{elem => println(elem.getAbsolutePath); elem}
        .filter(_.getPath.endsWith(".report"))
        //.map{elem => println(elem.getAbsolutePath); elem}
        .map(file => Try(Report.apply(file.getAbsolutePath)))
        .filter {
          case Failure(_) => false
          case Success(_) => true
        }
        .map(_.get)
        .foreach { report =>
          report.passReports.foreach { pass =>
            if (deltas.contains(pass.name)) {
              deltas(pass.name)
                .addOne(pass.start.until(pass.end, ChronoUnit.SECONDS))
            } else {
              deltas.addOne(
                pass.name -> ListBuffer(
                  pass.start.until(pass.end, ChronoUnit.SECONDS)))
            }
          }
        }
      deltas.foreach {
        case (str, value) =>
          val avg = value.sum.toDouble / value.length.toDouble
          val max = value.max
          val sigma = Math.sqrt(
            value
              .map(num => (num.toDouble - avg) * (num.toDouble - avg))
              .sum / (value.length - 1))
          println(
            s"$str ~ %.2f seconds (+/- %.2f/max %.2f)"
              .format(avg, sigma, max.toDouble))
      }
    }
  }

}
