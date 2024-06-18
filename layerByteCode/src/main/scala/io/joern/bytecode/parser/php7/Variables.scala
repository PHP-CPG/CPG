package io.joern.bytecode.parser.php7

import fastparse.NoWhitespace._
import fastparse._
import io.joern.bytecode.parser.constructs.Variable
import io.joern.bytecode.parser.php7.Basics.{anyNumber, legalIdentifier}

object Variables {

  def parseVariable[_: P]: P[String] =
    P("CV" ~ anyNumber.rep ~/ "($" ~ legalIdentifier.! ~ ")")

  def getVariable[_: P]: P[Variable] =
    P(parseVariable.map(Variable(_, tmp = false)))

  def parseTemporary[_: P]: P[String] = P(("T" ~ anyNumber.rep).!)

  def getTemporary[_: P]: P[Variable] =
    P(parseTemporary.map(Variable(_, tmp = true)))

  def parseReference[_: P]: P[String] = P(("V" ~ anyNumber.rep).!)

  def getReference[_: P]: P[Variable] =
    P(parseReference.map(x => Variable(x, tmp = true, reference = true)))

  def getAnyVariable[_: P]: P[Variable] =
    P(getVariable | getTemporary | getReference)

}
