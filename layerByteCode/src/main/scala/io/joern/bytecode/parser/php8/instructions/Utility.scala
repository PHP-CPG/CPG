package io.joern.bytecode.parser.php8.instructions

import fastparse.NoWhitespace._
import fastparse.{P, _}
import io.joern.bytecode.parser.constructs.Value
import io.joern.bytecode.parser.php8.Basics.anyNumber
import io.joern.bytecode.parser.php8.Literals.getAnyLiteral
import io.joern.bytecode.parser.php8.Variables.getAnyVariable

object Utility {

  def getAnyValue[_: P]: P[Value] = P(getAnyLiteral | getAnyVariable)

  def parseStringInQuotes[_: P]: P[String] =
    P("\"" ~ (!"\"" ~ AnyChar).rep.! ~ "\"")

  // replaces "("L" | "BB") ~ anyNumber.rep.!"
  def parseTarget[_: P]: P[String] = P("BB".? ~ anyNumber.rep(1).!)
}
