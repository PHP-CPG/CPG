package io.joern.bytecode.parser.php7.instructions

import fastparse.NoWhitespace._
import fastparse.{P, _}
import io.joern.bytecode.parser.constructs._
import io.joern.bytecode.parser.php7.Basics.anyNumber
import io.joern.bytecode.parser.php7.Variables.getAnyVariable
import io.joern.bytecode.parser.php7.instructions.Utility.getAnyValue

object Rope {

  def parseRopeInit[_: P]: P[(String, Value)] =
    P("ROPE_INIT " ~/ anyNumber.rep.! ~ " " ~ getAnyValue)
  def getRopeInit[_: P]: P[DualValueOperation] =
    P(parseRopeInit.map(x =>
      DualValueOperation("ROPE_INIT", IntegerLiteral(x._1.toLong), x._2)))

  def parseRopeAdd[_: P]: P[(String, Variable, Value)] =
    P("ROPE_ADD " ~/ anyNumber.rep.! ~ " " ~ getAnyVariable ~ " " ~ getAnyValue)
  def getRopeAdd[_: P]: P[TripleValueOperation] =
    P(
      parseRopeAdd.map(
        x =>
          TripleValueOperation("ROPE_ADD",
                               IntegerLiteral(x._1.toLong),
                               x._2,
                               x._3)))

  def parseRopeEnd[_: P]: P[(String, Variable, Value)] =
    P("ROPE_END " ~/ anyNumber.rep.! ~ " " ~ getAnyVariable ~ " " ~ getAnyValue)
  def getRopeEnd[_: P]: P[TripleValueOperation] =
    P(
      parseRopeEnd.map(
        x =>
          TripleValueOperation("ROPE_END",
                               IntegerLiteral(x._1.toLong),
                               x._2,
                               x._3)))

}
