package io.joern.bytecode.parser.php7

import fastparse.NoWhitespace._
import fastparse._
import io.joern.bytecode.parser.constructs._
import io.joern.bytecode.parser.php7.Basics.anyNumber
import io.joern.bytecode.parser.utils.decodeBase64

import java.nio.charset.StandardCharsets
import java.util.Base64

object Literals {

  def parseCharacterNotExiting[_: P]: P[String] =
    P(!"\")" ~ AnyChar.!)

  def parseStringLiteral[_: P]: P[String] =
    P("string(\"" ~ parseCharacterNotExiting.rep.! ~ "\")")

  def getStringLiteral[_: P]: P[StringLiteral] =
    P(parseStringLiteral.map(x => StringLiteral(decodeBase64(x))))

  def parseValidNumber[_: P]: P[Unit] = P("-".? ~ anyNumber.rep)

  def parseIntLiteral[_: P]: P[String] = P("int(" ~ parseValidNumber.! ~ ")")
  def getIntLiteral[_: P]: P[IntegerLiteral] =
    P(parseIntLiteral.map(x => IntegerLiteral(x.toLong)))

  def parseFLoatEString[_: P]: P[String] =
    P(("-".? ~ parseFloatNumberString ~ "e" ~ ("-" | "+") ~ anyNumber.rep).!)

  def parseFloatNumberString[_: P]: P[String] =
    P(("-".? ~ anyNumber.rep ~ ".".? ~ anyNumber.rep).!)

  def parseFloatLiteral[_: P]: P[String] =
    P("float(" ~/ ("nan".! | "-inf".! | "inf".! | parseFLoatEString | parseFloatNumberString) ~ ")")

  def getFloatLiteral[_: P]: P[FloatLiteral] =
    P(parseFloatLiteral.map(x =>
      FloatLiteral(x match {
        case "inf"  => Float.PositiveInfinity
        case "-inf" => Float.NegativeInfinity
        case "nan"  => Float.NaN
        case x      => x.toFloat
      })))

  def parseBooleanLiteral[_: P]: P[String] =
    P("bool(" ~/ ("true" | "false").! ~/ ")")

  def getBooleanLiteral[_: P]: P[BooleanLiteral] =
    P(parseBooleanLiteral.map(x => BooleanLiteral(x == "true")))

  def parseTryCatch[_: P]: P[String] =
    P("try-catch(" ~ anyNumber.! ~ ")")

  def getTryCatch[_: P]: P[TryCatchLiteral] =
    P(parseTryCatch.map(x => TryCatchLiteral(x.toInt)))

  def parseZvalLiteral[_: P]: P[String] =
    P("zval(type=" ~ anyNumber.rep.! ~ ")")

  def getZvalLiteral[_: P]: P[Zval] =
    P(parseZvalLiteral.map(x => Zval(x.toInt)))

  def parseNull[_: P]: P[Unit] = P("null")

  def getNull[_: P]: P[Null] = P(parseNull.map(_ => Null()))

  //def parseType[_: P]: P[String] = P("(" ~ ("long" | "int").! ~ ")")
  //def getType[_: P]: P[Type] = P(parseType.map(x => Type(x)))

  def parseArray[_: P]: P[Unit] = P("array(...)")

  def getArray[_: P]: P[ArrayValue] = P(parseArray.map(_ => ArrayValue(None)))

  def parseBytecodeKeyword[_: P]: P[String] =
    P(
      "(" ~
        ("self" |
          "parent" |
          "ref" |
          "array" |
          "double" |
          "string" |
          "long" |
          "int" |
          "object" |
          "bool" |
          "no-autolod" |
          "function" |
          "null" |
          "resource" |
          "isset" |
          "unqualified" |
          "in-namespace" |
          "empty" |
          "packed" |
          "exception" |
          "require_once" |
          "require" |
          "include_once" |
          "include" |
          "obj write" |
          "dim write" |
          "global+lock" |
          "global" |
          "value" |
          "eval" |
          "local" |
          "static").!
        ~ ")")

  def getByteCodeKeyword[_: P]: P[ByteCodeKeyword] =
    P(parseBytecodeKeyword.map(x => ByteCodeKeyword(x)))

  def parseByteCodeConstructor[_: P]: P[Unit] = {
    P("CONSTRUCTOR")
  }

  def getByteCodeConstructor[_: P]: P[Value] = {
    parseByteCodeConstructor.map { _ =>
      ByteCodeConstructor()
    }
  }

  def parseByteCodePlaceIndicator[_: P]: P[String] = P(
    "NEXT".! |
      "THIS".!
  )

  def getByteCodePlaceIndicator[_: P]: P[ByteCodePlaceIndicator] = P(
    parseByteCodePlaceIndicator.map(x => ByteCodePlaceIndicator(x))
  )

  def parseAssignOpCmdString[_: P]: P[String] =
    P("(" ~
      ("ADD" | "SUB" | "DIV" | "MUL" | "MOD" | "POW" | "SL" | "SR" | "CONCAT" | "BW_OR" | "BW_AND" | "BW_XOR").! ~
      ")")

  def getAssignOpCmd[_: P]: P[AssignOpLiteral] = {
    P(parseAssignOpCmdString.map(x => AssignOpLiteral(x)))
  }

  def getAnyLiteral[_: P]: P[Value] =
    P(getByteCodePlaceIndicator | getStringLiteral | getIntLiteral | getFloatLiteral
      | getBooleanLiteral | getByteCodeConstructor |
      getNull | getArray | getZvalLiteral | getByteCodeKeyword | getAssignOpCmd | getTryCatch)
}
