package io.joern.bytecode.parser.php8.instructions

import fastparse.NoWhitespace._
import fastparse._
import io.joern.bytecode.parser.constructs._
import io.joern.bytecode.parser.php8.Basics.anyNumber
import io.joern.bytecode.parser.php8.instructions.Utility.getAnyValue

object Array {

  def parseInitArray[_: P]: P[(String, Seq[Value])] =
    P("INIT_ARRAY " ~ anyNumber.rep.! ~ (" " ~ getAnyValue).rep)
  def getInitArray[_: P]: P[Opcode] =
    P(parseInitArray.map { x =>
      val pos = IntegerLiteral(x._1.toLong)
      x._2 match {
        case first :: Nil =>
          DualValueOperation("INIT_ARRAY", pos, first)
        case first :: second :: Nil =>
          TripleValueOperation("INIT_ARRAY", pos, first, second)
        case first :: second :: third :: Nil =>
          QuadrupleValueOperation("INIT_ARRAY", pos, first, second, third)
        case first :: second :: third :: fourth :: Nil =>
          QuintupleValueOperation("INIT_ARRAY",
                                  pos,
                                  first,
                                  second,
                                  third,
                                  fourth)
      }
    })

  def parseAddArrayElement[_: P]: P[Seq[Value]] =
    P("ADD_ARRAY_ELEMENT" ~ (" " ~ getAnyValue).rep)

  def getAddArrayElement[_: P]: P[Opcode] =
    P(parseAddArrayElement.map {
      case first :: second :: Nil =>
        DualValueOperation("ADD_ARRAY_ELEMENT", first, second)
      case first :: second :: third :: Nil =>
        TripleValueOperation("ADD_ARRAY_ELEMENT", first, second, third)
    })

  def parseInArray[_: P]: P[(String, Value, Value)] =
    P("IN_ARRAY " ~/ anyNumber.rep.! ~ " " ~ getAnyValue ~ " " ~ getAnyValue)
  def getInArray[_: P]: P[TripleValueOperation] =
    P(
      parseInArray.map(
        x =>
          TripleValueOperation("IN_ARRAY",
                               IntegerLiteral(x._1.toLong),
                               x._2,
                               x._3)))

  def parseAddArrayUnpack[_: P]: P[Value] =
    P("ADD_ARRAY_UNPACK" ~ " " ~ getAnyValue)
  def getAddArrayUnpack[_: P]: P[Opcode] =
    P(
      parseAddArrayUnpack.map(x => SingleValueOperation("ADD_ARRAY_UNPACK", x))
    )

}
