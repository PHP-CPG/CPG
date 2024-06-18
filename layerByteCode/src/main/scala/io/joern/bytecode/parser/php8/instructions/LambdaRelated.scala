package io.joern.bytecode.parser.php8.instructions

import fastparse.NoWhitespace._
import fastparse.{P, _}
import io.joern.bytecode.parser.constructs._
import io.joern.bytecode.parser.php8.Basics.anyNumber
import io.joern.bytecode.parser.php8.instructions.Utility.{
  getAnyValue,
  parseTarget
}

object LambdaRelated {

  def parseYield[_: P]: P[Seq[Value]] =
    P("YIELD" ~ (" " ~ getAnyValue).rep)
  def getYieldOpcode[_: P]: P[Opcode] =
    P(parseYield.map {
      case Nil                    => NoValueOperation("YIELD")
      case single :: Nil          => SingleValueOperation("YIELD", single)
      case first :: second :: Nil => DualValueOperation("YIELD", first, second)
    })

  def parseYieldFrom[_: P]: P[Seq[Value]] =
    P("YIELD_FROM" ~ (" " ~ getAnyValue).rep)
  def getYieldFrom[_: P]: P[Opcode] =
    P(parseYieldFrom.map {
      case single :: Nil => SingleValueOperation("YIELD_FROM", single)
    })

  def getYield[_: P]: P[Opcode] =
    P(
      &("YIELD_FROM ") ~ getYieldFrom |
        &("YIELD " | "YIELD") ~ getYieldOpcode
    )

  def parseTicks[_: P]: P[String] =
    P(&("TICKS ") ~ "TICKS" ~ " " ~ anyNumber.rep.!)
  def getTicks[_: P]: P[Opcode] =
    P(parseTicks.map(x =>
      SingleValueOperation("TICKS", IntegerLiteral(x.toLong))))

  def parseFastCall[_: P]: P[(String, Option[Value])] =
    P("FAST_CALL " ~ parseTarget ~ (" " ~ getAnyValue).?)
  def getFastCall[_: P]: P[Opcode] =
    P(parseFastCall.map {
      case (line, Some(value)) =>
        DualValueOperation("FAST_CALL", IntegerLiteral(line.toLong), value)
      case (line, None) =>
        SingleValueOperation("FAST_CALL", IntegerLiteral(line.toLong))
    })

}
