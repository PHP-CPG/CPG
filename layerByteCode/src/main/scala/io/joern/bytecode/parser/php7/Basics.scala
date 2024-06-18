package io.joern.bytecode.parser.php7

import fastparse.NoWhitespace._
import fastparse._

object Basics {

  def anyNonCapitalLetter[_: P]: P[Unit] = CharIn("a-z")

  def anyCapitalLetter[_: P]: P[Unit] = CharIn("A-Z")

  def anyLetter[_: P]: P[Unit] = P(anyNonCapitalLetter | anyCapitalLetter)

  def anyNumber[_: P]: P[Unit] = CharIn("0-9")

  //def legalIdentifier[_: P]: P[Unit] =
  //  P("_".? ~ (anyLetter | anyLetter | anyNumber | "_").rep)
  def legalIdentifier[_: P]: P[Unit] =
    P((!(" " | ")" | "(") ~ AnyChar).rep)

  def legalFileIdentifier[_: P]: P[Unit] =
    P((anyLetter | anyNumber | " " | "_" | "-" | ".").rep)

  def nonEscapedAnyChar[_: P]: P[Unit] = P(!"\\" ~ AnyChar)

  def escapedQuotation[_: P]: P[Unit] = P("\\\"")

  def escapedSlash[_: P]: P[Unit] = P("\\")

  def whiteSpaceAndWhiteSpaceControl[_: P]: P[Unit] =
    P('\n'.toString | '\t'.toString | CharIn(" "))

  def legalString[_: P]: P[Unit] =
    P("\"" ~
      (!"\"" ~ (escapedQuotation | whiteSpaceAndWhiteSpaceControl | escapedSlash | nonEscapedAnyChar)).rep
      ~ "\"")
}
