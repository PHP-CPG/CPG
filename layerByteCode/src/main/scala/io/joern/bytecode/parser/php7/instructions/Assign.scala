package io.joern.bytecode.parser.php7.instructions

import fastparse.NoWhitespace._
import fastparse._
import io.joern.bytecode.parser.constructs._
import io.joern.bytecode.parser.php7.Literals.getAssignOpCmd
import io.joern.bytecode.parser.php7.Variables.getAnyVariable
import io.joern.bytecode.parser.php7.instructions.Utility.getAnyValue

object Assign {

  def parseAssignDim[_: P]: P[(Variable, Value)] =
    P("ASSIGN_DIM " ~/ getAnyVariable ~ " " ~ getAnyValue)
  def getAssignDim[_: P]: P[DualValueOperation] =
    P(
      parseAssignDim.map(
        x =>
          DualValueOperation(
            "ASSIGN_DIM",
            x._1,
            x._2
        )))

  def parseAssign[_: P]: P[(Value, Value)] =
    P("ASSIGN " ~/ getAnyValue ~ " " ~/ getAnyValue)
  def getAssign[_: P]: P[DualValueOperation] =
    P(parseAssign.map(x => DualValueOperation("ASSIGN", x._1, x._2)))

  def parseAssignOp[_: P]: P[(Value, Value, Value)] =
    P(
      "ASSIGN_OP " ~/
        getAssignOpCmd ~/ " " ~
        getAnyValue ~ " " ~
        getAnyValue)
  def getAssignOp[_: P]: P[TripleValueOperation] =
    P(parseAssignOp.map(x =>
      TripleValueOperation("ASSIGN_OP", x._1, x._2, x._3)))

  def parseAssignStaticProp[_: P]: P[Seq[Value]] =
    P("ASSIGN_STATIC_PROP" ~/ (" " ~ getAnyValue).rep ~ &("\n" | End))
  def getAssignStaticProp[_: P]: P[Opcode] =
    P(parseAssignStaticProp.map {
      case first :: Nil => SingleValueOperation("ASSIGN_STATIC_PROP_1", first)
      case first :: second :: Nil =>
        DualValueOperation("ASSIGN_STATIC_PROP_2", first, second)
      case list =>
        throw new UnexpectedArgumentCount("ASSIGN_STATIC_PROP",
                                          Seq(1, 2),
                                          list.length)
    })

  def parseAssignStaticPropOp[_: P]: P[Seq[Value]] =
    P(&("ASSIGN_STATIC_PROP_OP ") ~ "ASSIGN_STATIC_PROP_OP" ~ (" " ~ getAnyValue).rep)
  def getAssignStaticPropOp[_: P]: P[Opcode] =
    P(parseAssignStaticPropOp.map {
      case first :: second :: Nil =>
        DualValueOperation("ASSIGN_STATIC_PROP_OP", first, second)
      case first :: second :: third :: Nil =>
        TripleValueOperation("ASSIGN_STATIC_PROP_OP", first, second, third)
      case list =>
        throw new UnexpectedArgumentCount("ASSIGN_STATIC_PROP_OP",
                                          Seq(2, 3),
                                          list.length)
    })

  def parseAssignStaticPropRef[_: P]: P[Seq[Value]] =
    P(&("ASSIGN_STATIC_PROP_REF ") ~ "ASSIGN_STATIC_PROP_REF" ~ (" " ~ getAnyValue).rep)
  def getAssignStaticPropRef[_: P]: P[Opcode] =
    P(parseAssignStaticPropRef.map {
      case first :: Nil =>
        SingleValueOperation("ASSIGN_STATIC_PROP_REF", first)
      case first :: second :: Nil =>
        DualValueOperation("ASSIGN_STATIC_PROP_REF", first, second)
      case list =>
        throw new UnexpectedArgumentCount("ASSIGN_STATIC_PROP_REF",
                                          Seq(1),
                                          list.length)
    })

  def parseAssignObj[_: P]: P[(Value, Value)] =
    P("ASSIGN_OBJ " ~ getAnyValue ~ " " ~ getAnyValue)
  def getAssignObj[_: P]: P[DualValueOperation] =
    P(parseAssignObj.map(x => DualValueOperation("ASSIGN_OBJ", x._1, x._2)))

  def parseAssignRef[_: P]: P[Seq[Value]] =
    P("ASSIGN_REF" ~ (" " ~ getAnyValue).rep ~ &("\n" | End)) // the End is needed to ensure that unit tests work
  def getAssignRef[_: P]: P[Opcode] =
    P(parseAssignRef.map {
      case first :: second :: Nil =>
        DualValueOperation("ASSIGN_REF_2", first, second)
      case first :: second :: third :: Nil =>
        TripleValueOperation("ASSIGN_REF_3", first, second, third)
    })

  def parseAssignObjRef[_: P]: P[Seq[Value]] =
    P("ASSIGN_OBJ_REF" ~ (" " ~ getAnyValue).rep ~ &("\n" | End))
  def getAssignObjRef[_: P]: P[Opcode] =
    P(parseAssignObjRef.map {
      case first :: second :: Nil =>
        DualValueOperation("ASSIGN_OBJ_REF_2", first, second)
      case first :: second :: third :: Nil =>
        TripleValueOperation("ASSIGN_OBJ_REF_3", first, second, third)
    })

  def parseAssignCommand[_: P]: P[Opcode] = P(
    &("ASSIGN ") ~ getAssign |
      &("ASSIGN_OBJ ") ~ getAssignObj |
      &("ASSIGN_DIM ") ~ getAssignDim |
      &("ASSIGN_STATIC_PROP ") ~ getAssignStaticProp |
      &("ASSIGN_STATIC_PROP_OP ") ~ getAssignStaticPropOp |
      &("ASSIGN_STATIC_PROP_REF ") ~ getAssignStaticPropRef |
      &("ASSIGN_OP ") ~ getAssignOp |
      &("ASSIGN_REF ") ~ getAssignRef |
      &("ASSIGN_OBJ_REF ") ~ getAssignObjRef
  )

}
