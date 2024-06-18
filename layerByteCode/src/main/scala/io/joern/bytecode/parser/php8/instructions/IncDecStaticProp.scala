package io.joern.bytecode.parser.php8.instructions

import fastparse.NoWhitespace._
import fastparse.{P, _}
import io.joern.bytecode.parser.constructs._
import io.joern.bytecode.parser.php8.instructions.Utility.getAnyValue
import io.joern.reporting.ReportableError

object IncDecStaticProp {

  def parseIncDecStaticProp[_: P]: P[(String, Seq[Value])] =
    P(&(("POST_INC" | "PRE_INC" | "POST_DEC" | "PRE_DEC") ~ "_STATIC_PROP ") ~ (("POST_INC" | "PRE_INC" | "POST_DEC" | "PRE_DEC") ~ "_STATIC_PROP").! ~ (" " ~ getAnyValue).rep)
  def getIncDecStaticProp[_: P]: P[Opcode] =
    P(parseIncDecStaticProp.map {
      case (opString, first :: Nil) => SingleValueOperation(opString, first)
      case (opString, first :: second :: Nil) =>
        DualValueOperation(opString, first, second)
      case x =>
        throw ReportableError(
          "",
          -1,
          "",
          "",
          s"when parsing IncDecStaticProp unexpected result tuple $x")
    })

}
