package io.joern.bytecode.parser.php7.instructions

import fastparse.NoWhitespace._
import fastparse.{P, _}
import io.joern.bytecode.parser.constructs.Value
import io.joern.bytecode.parser.php7.Literals.getAnyLiteral
import io.joern.bytecode.parser.php7.Variables.getAnyVariable
import io.joern.bytecode.parser.php8.Basics.anyNumber

object Utility {

  def getAnyValue[_: P]: P[Value] = P(getAnyLiteral | getAnyVariable)

  def parseStringInQuotes[_: P]: P[String] =
    P("\"" ~ (!"\"" ~ AnyChar).rep.! ~ "\"")

  def parseTarget[_: P]: P[String] = P(("BB" | "L") ~ anyNumber.rep(1).!)

}
