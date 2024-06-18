package io.joern.bytecode.parser.php8.instructions

import fastparse.NoWhitespace._
import fastparse.{P, _}
import io.joern.bytecode.parser.constructs._
import io.joern.bytecode.parser.php8.instructions.Utility.getAnyValue

object Generic {

  def parseNoValueCommandString[_: P]: P[Unit] = P(
    "DO_FCALL_BY_NAME" | "DO_ICALL" | "DO_UCALL" | "DO_FCALL" | "NOP" | "BEGIN_SILENCE" | "EXT_STMT" | "EXT_NOP" |
      "FETCH_THIS" | "GENERATOR_CREATE" | "GET_CALLED_CLASS" | "FUNC_NUM_ARGS" | "CHECK_UNDEF_ARGS" |
      "FETCH_GLOBALS" | 
      "VERIFY_NEVER_TYPE" |
      "CALLABLE_CONVERT" 
  )

  def parseNoValueCommand[_: P]: P[String] = P(parseNoValueCommandString.!)

  def getNoValueCommand[_: P]: P[NoValueOperation] =
    P(parseNoValueCommand.map(x => NoValueOperation(x)))

  def parseSingleValueCommandString[_: P]: P[Unit] = P(
    "ECHO " | "BW_NOT " | "BOOL_NOT " | "QM_ASSIGN " | "PRE_INC " | "POST_INC " | "PRE_DEC " | "POST_DEC " | "FREE " |
      "PRINT " | "FE_FREE " | "END_SILENCE " | "BOOL " | "OP_DATA " | "THROW " | "STRLEN " | "SEND_UNPACK " |
      "COUNT " | "DEFINED " | "GET_TYPE " | "UNSET_CV " | "COPY_TMP " |
      "CLONE " | "MAKE_REF " | "SEPARATE " | "DECLARE_LAMBDA_FUNCTION " | "GENERATOR_RETURN " | "DISCARD_EXCEPTION " |
      "CHECK_VAR " | "MATCH_ERROR " 
  )

  def parseSingleValueCommand[_: P]: P[(String, Value)] = P(
    parseSingleValueCommandString.! ~/ getAnyValue
  )

  def getSingleValueCommand[_: P]: P[SingleValueOperation] = P(
    parseSingleValueCommand.map(x =>
      SingleValueOperation(x._1.substring(0, x._1.length - 1), x._2))
  )

  def parseDualValueCommandString[_: P]: P[Unit] = P(
    "CONCAT " | "FAST_CONCAT " | "ADD " | "SUB " | "MUL " | "DIV " | "MOD " | "SL " | "SR " | "BW_OR " |
      "BW_AND " | "BW_XOR " | "BOOL_OR " | "IS_EQUAL " | "IS_NOT_EQUAL " | "IS_IDENTICAL " | "IS_NOT_IDENTICAL " |
      "IS_SMALLER " | "IS_SMALLER_OR_EQUAL " | "BIND_GLOBAL " | "DECLARE_CLASS_DELAYED " |
      "DECLARE_CONST " | "INCLUDE_OR_EVAL " | "FETCH_FUNC_ARG " | "FETCH_DIM_FUNC_ARG " | "POW " |
      "FETCH_DIM_R " | "FETCH_W " | "FETCH_DIM_W " | "ARRAY_KEY_EXISTS " | "FETCH_OBJ_RW " |
      "FETCH_OBJ_R " | "FETCH_RW " | "FETCH_OBJ_IS " | "FETCH_DIM_IS " | "FETCH_DIM_RW " |
      "UNSET_OBJ " | "FETCH_UNSET " | "UNSET_DIM " | "FETCH_DIM_UNSET " | "CASE " | "FETCH_OBJ_UNSET " | "UNSET_STATIC_PROP " |
      "POST_INC_OBJ " | "PRE_INC_OBJ " | "POST_DEC_OBJ " | "PRE_DEC_OBJ " | "BOOL_XOR " | "SPACESHIP " | "UNSET_VAR " |
      "CASE_STRICT "
  )

  def parseDualValueCommand[_: P]: P[(String, Value, Value)] =
    P(
      parseDualValueCommandString.! ~/
        getAnyValue ~
        " " ~
        getAnyValue)

  def getDualValueCommand[_: P]: P[DualValueOperation] =
    P(parseDualValueCommand.map(x =>
      DualValueOperation(x._1.substring(0, x._1.length - 1), x._2, x._3)))

  def parseTripleValueCommandString[_: P]: P[Unit] = P(
    "ASSIGN_DIM_OP " | "ASSIGN_OBJ_OP " | "ISSET_ISEMPTY_VAR "
  )

  def parseTripleValueCommand[_: P]: P[(String, Value, Value, Value)] =
    P(parseTripleValueCommandString.! ~/ getAnyValue ~ " " ~ getAnyValue ~ " " ~ getAnyValue)

  def getTripleValueCommand[_: P]: P[TripleValueOperation] =
    P(
      parseTripleValueCommand.map(
        x =>
          TripleValueOperation(x._1.substring(0, x._1.length - 1),
                               x._2,
                               x._3,
                               x._4)))

  def parseQuadrupleValueCommandString[_: P]: P[Unit] = P(
    "DOESNOTEXIST "
  )

  def parseQuadrupleValueCommand[_: P]
    : P[(String, Value, Value, Value, Value)] = P(
    parseQuadrupleValueCommandString.! ~/ getAnyValue ~ " " ~ getAnyValue ~ " " ~ getAnyValue ~ " " ~ getAnyValue
  )

  def getQuadrupleValueCommand[_: P]: P[QuadrupleValueOperation] = {
    parseQuadrupleValueCommand.map(
      x =>
        QuadrupleValueOperation(x._1.substring(0, x._1.length - 1),
                                x._2,
                                x._3,
                                x._4,
                                x._5))
  }
}
