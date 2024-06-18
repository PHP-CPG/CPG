package parser.php

import org.apache.commons.io.FileUtils

import scala.sys.process.{ProcessLogger, _}
import scala.util.parsing.json._

object FileParser {

  val defaultPhpParserPath
    : String = FileUtils.getUserDirectory.toString + "/.config/composer/vendor/bin/php-parse"

  def parse(
      str: String,
      phpParser: String = defaultPhpParserPath): List[Map[String, Any]] = {
    if (new java.io.File(str).exists()) {
      val cmd = s"$phpParser --json-dump $str"
      val stdout = new StringBuilder
      val stderr = new StringBuilder
      cmd ! ProcessLogger(stdout append _, stderr append _)
      JSON.parseFull(stdout.toString()) match {
        case Some(x) =>
          x match {
            case x: List[Map[String, Any]] => x
            case _ =>
              throw new RuntimeException(
                s"return of json parsing was unexpected ${x.getClass.toString}")
          }
        case None =>
          throw new RuntimeException(s"unable to parse file $str successfully")
      }
    } else {
      throw new RuntimeException(
        s"file $str does not exist and I only support file parsing")
    }
  }

}
