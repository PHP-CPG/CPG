package io.joern.bytecode.parser.php8.instructions

import fastparse.NoWhitespace._
import fastparse._
import io.joern.bytecode.parser.constructs._
import io.joern.bytecode.parser.php8.Basics.anyNumber
import io.joern.bytecode.parser.php8.instructions.Utility.{
  getAnyValue,
  parseStringInQuotes,
  parseTarget
}
import io.joern.bytecode.parser.utils.decodeBase64

object ControlConstructs {

  def parseStringDestinationPattern[_: P]: P[(String, String)] =
    P(
      (parseStringInQuotes
        .map(decodeBase64) | "default".!) ~/ ": " ~ parseTarget)
  def parseSwitchString[_: P]
    : P[(Value, (String, String), Seq[(String, String)])] =
    P("SWITCH_STRING " ~/ getAnyValue ~ " " ~ parseStringDestinationPattern ~ ("," ~ " " ~ parseStringDestinationPattern).rep)
  def getSwitchString[_: P]: P[Opcode] =
    P(
      parseSwitchString.map(
        x =>
          SWITCH("SWITCH_STRING",
                 x._1,
                 (Seq(x._2) ++ x._3).map(x => (x._1, x._2.toInt)))))

  def parseNumberDestinationPattern[_: P]: P[(String, String)] =
    P(("default".! | ("-".? ~ anyNumber.rep).!) ~ ": " ~ parseTarget)
  def parseSwitchLong[_: P]
    : P[(Value, (String, String), Seq[(String, String)])] =
    P("SWITCH_LONG " ~/ getAnyValue ~ " " ~ parseNumberDestinationPattern ~ ("," ~ " " ~ parseNumberDestinationPattern).rep)
  def getSwitchLong[_: P]: P[Opcode] =
    P(
      parseSwitchLong.map(
        x =>
          SWITCH("SWITCH_LONG",
                 x._1,
                 (Seq(x._2) ++ x._3).map(x => (x._1, x._2.toInt)))))

  def getSwitchStatement[_: P]: P[Opcode] =
    P(
      &("SWITCH_STRING ") ~ getSwitchString |
        &("SWITCH_LONG ") ~ getSwitchLong
    )

  def parseExit[_: P]: P[Option[Value]] =
    P("EXIT" ~ (" " ~ getAnyValue).?)

  def getExit[_: P]: P[Opcode] =
    P(parseExit.map {
      case Some(x) => SingleValueOperation("EXIT", x)
      case None    => NoValueOperation("EXIT")
    })

  def parseCatch[_: P]: P[(Value, Option[String])] =
    P("CATCH " ~ getAnyValue ~ (" " ~ parseTarget).?)
  def getCatch[_: P]: P[Opcode] =
    P(parseCatch.map {
      case (value, Some(x)) =>
        DualValueOperation("CATCH", value, IntegerLiteral(x.toLong))
      case (value, None) => SingleValueOperation("CATCH", value)
    })
}
