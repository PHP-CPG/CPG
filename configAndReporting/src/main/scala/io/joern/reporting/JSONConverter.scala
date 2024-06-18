package io.joern.reporting

import spray.json.{
  DefaultJsonProtocol,
  JsArray,
  JsBoolean,
  JsObject,
  JsString,
  JsValue,
  RootJsonFormat,
  enrichAny
}

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.collection.mutable.ListBuffer

object JSONConverter extends DefaultJsonProtocol {

  implicit val passWarningFormat: RootJsonFormat[PassWarning] = jsonFormat5(
    PassWarning)

  implicit val passErrorFormat: RootJsonFormat[PassError] = jsonFormat6(
    PassError)

  implicit object LocalDateTimeJSONFormat
      extends RootJsonFormat[LocalDateTime] {
    val sdf2: DateTimeFormatter =
      DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSS")

    override def write(obj: LocalDateTime): JsValue = {
      JsString(sdf2.format(obj))
    }

    override def read(json: JsValue): LocalDateTime = {
      LocalDateTime.from(sdf2.parse(json.asInstanceOf[JsString].value))
    }

  }

  implicit val passReportFormat: RootJsonFormat[PassReport] = jsonFormat5(
    PassReport)

  implicit val linkingReportFormat: RootJsonFormat[LinkingReport] = jsonFormat5(
    LinkingReport)

  implicit object ReportFormat extends RootJsonFormat[Report] {
    override def write(obj: Report): JsValue = {
      JsObject(
        "success" -> JsBoolean(obj.successWithinSpecs),
        "passes" -> JsArray(obj.passReports.map(_.toJson).toVector),
        "linker" -> obj.linkingReport.toJson
      )
    }

    override def read(json: JsValue): Report = {
      val content = json.asJsObject.fields
      val lb = ListBuffer(
        content("passes")
          .asInstanceOf[JsArray]
          .elements
          .map(_.convertTo[PassReport])
          .toList: _*)
      new Report(
        lb,
        Some(content("linker").asJsObject.convertTo[LinkingReport])
      )
    }
  }

}
