package io.joern.bytecode.parser.php7.instructions

import fastparse.NoWhitespace._
import fastparse.{P, _}
import io.joern.bytecode.parser.constructs._
import io.joern.bytecode.parser.php7.instructions.Utility.getAnyValue

object ClassRelated {

  def parseGetClass[_: P]: P[Seq[Value]] =
    P(&("GET_CLASS") ~ "GET_CLASS" ~ (" " ~ getAnyValue).rep)
  def getGetClass[_: P]: P[Opcode] =
    P(parseGetClass.map {
      case Nil           => NoValueOperation("GET_CLASS")
      case single :: Nil => SingleValueOperation("GET_CLASS", single)
    })

  def parseDeclareAnonClass[_: P]: P[Seq[Value]] =
    P(&("DECLARE_ANON_CLASS ") ~ "DECLARE_ANON_CLASS" ~ (" " ~ getAnyValue).rep)
  def getDeclareAnonClass[_: P]: P[Opcode] =
    P(
      parseDeclareAnonClass.map {
        case first :: Nil => SingleValueOperation("DECLARE_ANON_CLASS", first)
        case first :: second :: Nil =>
          DualValueOperation("DECLARE_ANON_CLASS", first, second)
      }
    )

  def parseDeclareClass[_: P]: P[Seq[Value]] =
    P(
      &("DECLARE_CLASS ") ~ "DECLARE_CLASS" ~ (" " ~ getAnyValue).rep ~ &(
        "\n" | End))
  def getDeclareClass[_: P]: P[Opcode] =
    P(parseDeclareClass.map {
      case single :: Nil => SingleValueOperation("DECLARE_CLASS", single)
      case first :: second :: Nil =>
        DualValueOperation("VERIFY_RETURN_TYPE", first, second)
    })

}
