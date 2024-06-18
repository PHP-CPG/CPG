package io.joern.bytecode.parser.php7

import fastparse.NoWhitespace._
import fastparse._
import io.joern.bytecode.parser.constructs.{
  MethodHeader,
  MethodHeaderMetaFileInfo,
  MethodHeaderMetaParsingInfo,
  MethodHeaderRangeLine
}
import io.joern.bytecode.parser.php7.Basics.{anyLetter, anyNumber}

object HeaderBlock {

  def parseValidNameSpace[_: P]: P[String] = P((anyLetter | "\\").rep.! ~ "\\")

  def parseValidClassname[_: P]: P[String] =
    P((("_" | anyLetter) ~ ("_" | anyLetter | anyNumber).rep).! ~ "::")

  def parseHeaderBlockMethodIdentifier[_: P]: P[String] =
    P(("::" | !(" " | ":") ~ AnyChar).rep.!)

  def getHeaderBlockMethodIdentifier[_: P]
    : P[(Option[String], Option[String], String)] =
    P(parseHeaderBlockMethodIdentifier.map(x => {
      x.split("::").toList match {
        case classString :: methodString :: Nil =>
          val lastSlash = classString.lastIndexOf("\\")
          if (lastSlash == -1) {
            (None, Some(classString), methodString)
          } else {
            val (namespace, classname) = classString.splitAt(lastSlash)
            (if (namespace == "") None else Some(namespace),
             Some(classname.substring(1)),
             methodString)
          }
        case methodString :: Nil =>
          val lastSlash = methodString.lastIndexOf("\\")
          if (lastSlash == -1) {
            (None, None, methodString)
          } else {
            val (namespace, methodname) = methodString.splitAt(lastSlash)
            (if (namespace == "") None else Some(namespace),
             None,
             methodname.substring(1))
          }
        case _ => throw new RuntimeException("unexpected")
      }
    }))

  def parseHeaderBlockLinesValue[_: P]: P[String] =
    P("lines" ~ "=" ~ anyNumber.rep.!)

  def getHeaderBLockLinesValue[_: P]: P[Int] =
    P(parseHeaderBlockLinesValue.map(_.toInt))

  def parseHeaderBlockArgsValue[_: P]: P[String] =
    P("args" ~ "=" ~ anyNumber.rep.!)

  def getHeaderBlocArgsValue[_: P]: P[Int] =
    P(parseHeaderBlockArgsValue.map(_.toInt))

  def parseHeaderBlockVarsValue[_: P]: P[String] =
    P("vars" ~ "=" ~ anyNumber.rep.!)

  def getHeaderBlockVarsValue[_: P]: P[Int] =
    P(parseHeaderBlockVarsValue.map(_.toInt))

  def parseHeaderBlockTmpsValue[_: P]: P[String] =
    P("tmps" ~ "=" ~ anyNumber.rep.!)

  def getHeaderBlockTmpsValue[_: P]: P[Int] =
    P(parseHeaderBlockTmpsValue.map(_.toInt))

  def parseHeaderBlockMethodMetaBlock[_: P]: P[(Int, Int, Int, Int)] =
    P(
      "(" ~ getHeaderBLockLinesValue ~
        ", " ~ getHeaderBlocArgsValue ~
        ", " ~ getHeaderBlockVarsValue ~
        ", " ~ getHeaderBlockTmpsValue ~ ")")

  def parseHeaderBlockMethodDefinitionLine[_: P]
    : P[(Option[String], Option[String], String, (Int, Int, Int, Int))] =
    P(getHeaderBlockMethodIdentifier ~ ":" ~ " ; " ~ parseHeaderBlockMethodMetaBlock)

  def getHeaderBLockMethodDefinitionLine[_: P]: P[MethodHeader] =
    P(
      parseHeaderBlockMethodDefinitionLine.map(
        result =>
          MethodHeader(result._3,
                       result._2,
                       result._1,
                       result._4._1,
                       result._4._2,
                       result._4._3,
                       result._4._4)))

  // time definition
  def parseHeaderBlockMetaLineParsingWord[_: P]: P[Unit] =
    P("before" | "optimizer" | "block" | "pass")

  def parseHeaderBlockMetaLineParsing[_: P]: P[Seq[String]] =
    P(" ".rep ~ ";" ~ " " ~ "(" ~ (parseHeaderBlockMetaLineParsingWord.! ~ " ".?).rep ~ ")")

  def getHeaderBlockMetaLineParsing[_: P]: P[MethodHeaderMetaParsingInfo] =
    P(parseHeaderBlockMetaLineParsing.map(x => MethodHeaderMetaParsingInfo(x)))

  def newline[_: P]: P[Unit] = P("\n" | "\r\n" | "\r" | "\f")

  def parseHeaderBlockMetaLineFileInfo[_: P]: P[String] =
    P(" ".rep ~ ";" ~ " " ~ (!newline ~ AnyChar).rep.!)
  def getHeaderBlockMetaLineFileInfo[_: P]: P[MethodHeaderMetaFileInfo] =
    P(parseHeaderBlockMetaLineFileInfo.map(x => MethodHeaderMetaFileInfo(x)))

  def parseHeaderBlockRangeLine[_: P]: P[(String, String)] =
    P(" ".rep ~ "; " ~ "return".! ~ (!"\n" ~ AnyChar).rep.!)

  def getHeaderBlockRangeLine[_: P]: P[MethodHeaderRangeLine] =
    parseHeaderBlockRangeLine
      .map(x => x._1 + x._2)
      .map(MethodHeaderRangeLine)

  def getHeaderBlock[_: P]
    : P[(MethodHeader, MethodHeaderMetaParsingInfo, MethodHeaderMetaFileInfo)] =
    P(
      getHeaderBLockMethodDefinitionLine ~ "\n" ~
        getHeaderBlockMetaLineParsing ~ "\n" ~
        getHeaderBlockMetaLineFileInfo ~ "\n")

}
