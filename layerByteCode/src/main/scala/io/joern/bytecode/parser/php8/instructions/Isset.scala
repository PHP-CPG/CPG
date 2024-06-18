package io.joern.bytecode.parser.php8.instructions

import fastparse.NoWhitespace._
import fastparse._
import io.joern.bytecode.parser.constructs._
import io.joern.bytecode.parser.php8.Literals.{
  getByteCodeKeyword,
  getStringLiteral
}
import io.joern.bytecode.parser.php8.Variables.getAnyVariable
import io.joern.bytecode.parser.php8.instructions.Utility.getAnyValue

object Isset {

  def parseIssetIsemptyVar[_: P]: P[(ByteCodeKeyword, ByteCodeKeyword, Value)] =
    P(
      "ISSET_ISEMPTY_VAR " ~/ getByteCodeKeyword ~ " " ~ getByteCodeKeyword ~
        " " ~ (getAnyVariable | getStringLiteral))
  def getIssetIsemptyVar[_: P]: P[TripleValueOperation] =
    P(parseIssetIsemptyVar.map(x =>
      TripleValueOperation("ISSET_ISEMPTY_VAR", x._1, x._2, x._3)))

  def parseIssetIsemptyDimObj[_: P]: P[(ByteCodeKeyword, Value, Value)] =
    P("ISSET_ISEMPTY_DIM_OBJ " ~/ getByteCodeKeyword ~ " " ~ getAnyValue ~ " " ~ getAnyValue)
  def getIssetIsemptyDimObj[_: P]: P[TripleValueOperation] =
    P(parseIssetIsemptyDimObj.map(x =>
      TripleValueOperation("ISSET_ISEMPTY_DIM_OBJ", x._1, x._2, x._3)))

  def parseIssetIsemptyCv[_: P]: P[(ByteCodeKeyword, Variable)] =
    P("ISSET_ISEMPTY_CV " ~/ getByteCodeKeyword ~ " " ~ getAnyVariable)
  def getIssetIsemptyCv[_: P]: P[DualValueOperation] =
    P(parseIssetIsemptyCv.map(x =>
      DualValueOperation("ISSET_ISEMPTY_CV", x._1, x._2)))

  def parseIssetIsemptyPropObj[_: P]: P[(ByteCodeKeyword, Value, Value)] =
    P("ISSET_ISEMPTY_PROP_OBJ " ~ getByteCodeKeyword ~ " " ~ getAnyValue ~ " " ~ getAnyValue)
  def getIssetIsemptyPropObj[_: P]: P[TripleValueOperation] =
    P(
      parseIssetIsemptyPropObj.map(
        x => TripleValueOperation("ISSET_ISEMPTY_PROP_OBJ", x._1, x._2, x._3)
      ))

  def parseIssetIsEmptyStaticProp[_: P]: P[Seq[Value]] =
    P(&("ISSET_ISEMPTY_STATIC_PROP ") ~ "ISSET_ISEMPTY_STATIC_PROP" ~ (" " ~ getAnyValue).rep)
  def getIssetIsEmptyStaticProp[_: P]: P[Opcode] =
    P(parseIssetIsEmptyStaticProp.map {
      case first :: second :: third :: Nil =>
        TripleValueOperation("ISSET_ISEMPTY_STATIC_PROP", first, second, third)
      case first :: second :: third :: fourth :: Nil =>
        QuadrupleValueOperation("ISSET_ISEMPTY_STATIC_PROP",
                                first,
                                second,
                                third,
                                fourth)
    })

  def parseIssetIsemptyThis[_: P]: P[String] = P("ISSET_ISEMPTY_THIS".!)
  def getIssetIsemptyThis[_: P]: P[NoValueOperation] =
    P(parseIssetIsemptyThis.map { _ =>
      NoValueOperation("ISSET_ISEMPTY_THIS")
    })

  def getIssetCommand[_: P]: P[Opcode] =
    P(
      getIssetIsemptyVar |
        getIssetIsemptyCv |
        getIssetIsemptyDimObj |
        getIssetIsemptyPropObj |
        getIssetIsEmptyStaticProp |
        getIssetIsemptyThis)

}
