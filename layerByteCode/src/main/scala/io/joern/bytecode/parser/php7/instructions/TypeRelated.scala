package io.joern.bytecode.parser.php7.instructions

import fastparse.NoWhitespace._
import fastparse._
import io.joern.bytecode.parser.constructs._
import io.joern.bytecode.parser.php7.Basics.anyLetter
import io.joern.bytecode.parser.php7.instructions.Utility.{
  getAnyValue,
  parseTarget
}

object TypeRelated {

  def parseCastTypeString[_: P]: P[String] = P("(" ~ anyLetter.rep.! ~ ")")
  def parseCast[_: P]: P[(Object, Value)] =
    P("CAST " ~ (parseCastTypeString | getAnyValue) ~ " " ~ getAnyValue)
  def getCast[_: P]: P[DualValueOperation] =
    P(parseCast.map(x =>
      x._1 match {
        case value: String =>
          DualValueOperation("CAST", StringLiteral(value), x._2)
        case value: Value => DualValueOperation("CAST", value, x._2)
    }))

  def parseInstanceOf[_: P]: P[Seq[Value]] =
    P(&("INSTANCEOF ") ~ "INSTANCEOF" ~ (" " ~ getAnyValue).rep)
  def getInstanceOf[_: P]: P[Opcode] =
    P(parseInstanceOf.map {
      case first :: second :: Nil =>
        DualValueOperation("INSTANCEOF", first, second)
      case first :: second :: third :: fourth :: Nil =>
        QuadrupleValueOperation("INSTANCEOF", first, second, third, fourth)
    })

  def parseCoalesce[_: P]: P[(Value, String)] =
    P("COALESCE " ~/ getAnyValue ~ " " ~ parseTarget)
  def getCoalesce[_: P]: P[Opcode] =
    P(parseCoalesce.map(x =>
      DualValueOperation("COALESCE", x._1, IntegerLiteral(x._2.toLong))))

}
