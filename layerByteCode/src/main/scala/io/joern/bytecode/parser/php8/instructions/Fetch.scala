package io.joern.bytecode.parser.php8.instructions

import fastparse.NoWhitespace._
import fastparse._
import io.joern.bytecode.parser.constructs._
import io.joern.bytecode.parser.php8.Literals.{
  getByteCodeKeyword,
  getStringLiteral
}
import io.joern.bytecode.parser.php8.instructions.Utility.getAnyValue

object Fetch {

  def parseFetchObjFuncArgA[_: P]: P[(Option[ByteCodeKeyword], Value, Value)] =
    P("FETCH_OBJ_FUNC_ARG " ~ (getByteCodeKeyword ~ " ").? ~ getAnyValue ~ " " ~ getAnyValue)
  def getFetchObjFuncArgA[_: P]: P[Opcode] =
    P(parseFetchObjFuncArgA.map(x =>
      x._1 match {
        case Some(keyword) =>
          TripleValueOperation("FETCH_OBJ_FUNC_ARG", keyword, x._2, x._3)
        case None => DualValueOperation("FETCH_OBJ_FUNC_ARG", x._2, x._3)
    }))

  def parseFetchObjFuncArgB[_: P]: P[(ByteCodeKeyword, StringLiteral)] =
    P("FETCH_OBJ_FUNC_ARG " ~ getByteCodeKeyword ~ " " ~ "THIS" ~ " " ~ getStringLiteral)
  def getFetchObjFuncArgB[_: P]: P[TripleValueOperation] =
    P(
      parseFetchObjFuncArgB.map(
        x =>
          TripleValueOperation("FETCH_OBJ_FUNC_ARG",
                               x._1,
                               StringLiteral("THIS"),
                               x._2)))

  def getFetchObjFuncArg[_: P]: P[Opcode] =
    P(getFetchObjFuncArgA | getFetchObjFuncArgB)

  def parseStaticPropR[_: P]: P[Seq[Value]] =
    P("FETCH_STATIC_PROP_R" ~/ (" " ~ getAnyValue).rep)
  def getStaticPropR[_: P]: P[Opcode] =
    P(parseStaticPropR.map {
      case first :: second :: Nil =>
        DualValueOperation("FETCH_STATIC_PROP_R", first, second)
      case first :: second :: third :: Nil =>
        TripleValueOperation("FETCH_STATIC_PROP_R", first, second, third)
    })

  def parseStaticPropW[_: P]: P[Seq[Value]] =
    P("FETCH_STATIC_PROP_W" ~/ (" " ~ getAnyValue).rep)
  def getStaticPropW[_: P]: P[Opcode] =
    parseStaticPropW.map {
      case first :: second :: third :: fourth :: Nil =>
        QuadrupleValueOperation("FETCH_STATIC_PROP_W",
                                first,
                                second,
                                third,
                                fourth)
      case first :: second :: third :: Nil =>
        TripleValueOperation("FETCH_STATIC_PROP_W", first, second, third)
      case first :: second :: Nil =>
        DualValueOperation("FETCH_STATIC_PROP_W", first, second)
    }

  def parseStaticPropFuncArg[_: P]: P[Seq[Value]] =
    P(&("FETCH_STATIC_PROP_FUNC_ARG ") ~ "FETCH_STATIC_PROP_FUNC_ARG" ~/ (" " ~ getAnyValue).rep)
  def getStaticPropFuncArg[_: P]: P[Opcode] =
    P(parseStaticPropFuncArg.map {
      case first :: second :: Nil =>
        DualValueOperation("FETCH_STATIC_PROP_FUNC_ARG", first, second)
      case first :: second :: third :: Nil =>
        TripleValueOperation("FETCH_STATIC_PROP_FUNC_ARG", first, second, third)
      case first :: second :: third :: fourth :: Nil =>
        QuadrupleValueOperation("FETCH_STATIC_PROP_FUNC_ARG",
                                first,
                                second,
                                third,
                                fourth)
    })

  def parseFetchStaticPropIs[_: P]: P[Seq[Value]] =
    P("FETCH_STATIC_PROP_IS" ~/ (" " ~ getAnyValue).rep)
  def getFetchStaticPropIs[_: P]: P[Opcode] =
    P(parseFetchStaticPropIs.map {
      case first :: second :: Nil =>
        DualValueOperation("FETCH_STATIC_PROP_IS", first, second)
      case first :: second :: third :: Nil =>
        TripleValueOperation("FETCH_STATIC_PROP_IS", first, second, third)
    })

  def parseFetchDimR[_: P]: P[(Value, Value)] =
    P("FETCH_DIM_R " ~/ getAnyValue ~ " " ~/ getAnyValue)
  def getFetchDimR[_: P]: P[DualValueOperation] =
    P(parseFetchDimR.map(x => DualValueOperation("FETCH_DIM_R", x._1, x._2)))

  def parseFetchListR[_: P]: P[(Value, Value)] =
    P("FETCH_LIST_R " ~/ getAnyValue ~ " " ~ getAnyValue)
  def getFetchListR[_: P]: P[DualValueOperation] =
    P(parseFetchListR.map(x => DualValueOperation("FETCH_LIST_R", x._1, x._2)))

  def parseFetchClassConstantA[_: P]
    : P[(ByteCodeKeyword, ByteCodeKeyword, StringLiteral)] =
    P("FETCH_CLASS_CONSTANT " ~ getByteCodeKeyword ~ " " ~ getByteCodeKeyword ~ " " ~ getStringLiteral)
  def getFetchClassConstantA[_: P]: P[TripleValueOperation] =
    P(parseFetchClassConstantA.map(x =>
      TripleValueOperation("FETCH_CLASS_CONSTANT", x._1, x._2, x._3)))

  def parseFetchClassConstantB[_: P]: P[(Value, Value)] =
    P("FETCH_CLASS_CONSTANT " ~ getAnyValue ~ " " ~ getAnyValue)
  def getFetchClassConstantB[_: P]: P[DualValueOperation] =
    P(parseFetchClassConstantB.map(x =>
      DualValueOperation("FETCH_CLASS_CONSTANT", x._1, x._2)))

  def parseFetchClass[_: P]: P[Seq[Value]] =
    P("FETCH_CLASS" ~ (" " ~ getAnyValue).rep)
  def getFetchClass[_: P]: P[Opcode] =
    P(parseFetchClass.map {
      case first :: second :: Nil =>
        DualValueOperation("FETCH_CLASS", first, second)
      case first :: second :: third :: Nil =>
        TripleValueOperation("FETCH_CLASS", first, second, third)
      case first :: second :: third :: fourth :: Nil =>
        QuadrupleValueOperation("FETCH_CLASS", first, second, third, fourth)
    })

  def parseFetchClassName[_: P]: P[Value] =
    P("FETCH_CLASS_NAME" ~ " " ~ getAnyValue)
  def getFetchClassName[_: P]: P[Opcode] =
    P(parseFetchClassName.map(x => SingleValueOperation("FETCH_CLASS_NAME", x)))

  def getFetchClassConstant[_: P]: P[Opcode] =
    P(getFetchClassConstantA | getFetchClassConstantB)

  def parseFetchDimFuncArg[_: P]: P[(Value, Value)] =
    P("FETCH_DIM_FUNC_ARG " ~/ getAnyValue ~ " " ~ getAnyValue)
  def getFetchDimFuncArg[_: P]: P[DualValueOperation] =
    P(parseFetchDimFuncArg.map(x =>
      DualValueOperation("FETCH_DIM_FUNC_ARG", x._1, x._2)))

  def parseFetchConstant[_: P]: P[Seq[Value]] =
    P("FETCH_CONSTANT" ~/ (" " ~ getAnyValue).rep(1))

  def getFetchConstant[_: P]: P[Opcode] = P(
    parseFetchConstant.map {
      case first :: Nil => SingleValueOperation("FETCH_CONSTANT", first)
      case first :: second :: Nil =>
        DualValueOperation("FETCH_CONSTANT", first, second)
      case first :: second :: third :: Nil =>
        TripleValueOperation("FETCH_CONSTANT", first, second, third)
    }
  )

  def parseFetchR[_: P]: P[(ByteCodeKeyword, Value)] =
    P(
      "FETCH_R " ~ getByteCodeKeyword ~ " " ~ getAnyValue
    )
  def getFetchR[_: P]: P[DualValueOperation] =
    P(
      parseFetchR.map(
        x => DualValueOperation("FETCH_R", x._1, x._2)
      ))

  def parseFetchIs[_: P]: P[(ByteCodeKeyword, Value)] =
    P(
      "FETCH_IS " ~ getByteCodeKeyword ~ " " ~ getAnyValue
    )
  def getFetchIs[_: P]: P[DualValueOperation] =
    P(
      parseFetchIs.map(
        x => DualValueOperation("FETCH_IS", x._1, x._2)
      )
    )

  def parseFetchObjW[_: P]: P[Seq[Value]] =
    P("FETCH_OBJ_W" ~ (" " ~ getAnyValue).rep ~ &("\n" | End))
  def getFetchObjW[_: P]: P[Opcode] =
    P(parseFetchObjW.map {
      case first :: second :: Nil =>
        DualValueOperation("FETCH_OBJ_W_2", first, second)
      case first :: second :: third :: Nil =>
        TripleValueOperation("FETCH_OBJ_W_3", first, second, third)
    })

  def parseFetchStaticPropUnset[_: P]: P[Seq[Value]] =
    P("FETCH_STATIC_PROP_UNSET" ~ (" " ~ getAnyValue).rep)
  def getFetchStaticPropUnset[_: P]: P[Opcode] =
    P(parseFetchStaticPropUnset.map {
      case first :: second :: third :: Nil =>
        TripleValueOperation("FETCH_STATIC_PROP_UNSET", first, second, third)
      case first :: second :: Nil =>
        DualValueOperation("FETCH_STATIC_PROP_UNSET", first, second)
    })

  def parseFetchStaticPropRw[_: P]: P[Seq[Value]] =
    P("FETCH_STATIC_PROP_RW" ~ (" " ~ getAnyValue).rep)
  def getFetchStaticPropRw[_: P]: P[Opcode] =
    P(parseFetchStaticPropRw.map {
      case first :: second :: Nil =>
        DualValueOperation("FETCH_STATIC_PROP_RW", first, second)
      case first :: second :: third :: Nil =>
        TripleValueOperation("FETCH_STATIC_PROP_RW", first, second, third)
    })

  def parseFetchCommand[_: P]: P[Opcode] = P(
    &("FETCH_OBJ_FUNC_ARG ") ~ getFetchObjFuncArg |
      &("FETCH_DIM_FUNC_ARG") ~ getFetchDimFuncArg |
      &("FETCH_STATIC_PROP_FUNC_ARG ") ~ getStaticPropFuncArg |
      &("FETCH_STATIC_PROP_R ") ~ getStaticPropR |
      &("FETCH_STATIC_PROP_W ") ~ getStaticPropW |
      &("FETCH_STATIC_PROP_IS ") ~ getFetchStaticPropIs |
      &("FETCH_STATIC_PROP_UNSET ") ~ getFetchStaticPropUnset |
      &("FETCH_STATIC_PROP_RW ") ~ getFetchStaticPropRw |
      //&("FETCH_OBJ_R ") ~ getFetchObjR |
      &("FETCH_CLASS_NAME ") ~ getFetchClassName |
      &("FETCH_DIM_R ") ~ getFetchDimR |
      &("FETCH_LIST_R ") ~ getFetchListR |
      &("FETCH_CLASS ") ~ getFetchClass |
      &("FETCH_CLASS_CONSTANT ") ~ getFetchClassConstant |
      &("FETCH_CONSTANT ") ~ getFetchConstant |
      &("FETCH_R ") ~ getFetchR |
      &("FETCH_IS ") ~ getFetchIs |
      &("FETCH_OBJ_W ") ~ getFetchObjW
  )

}
