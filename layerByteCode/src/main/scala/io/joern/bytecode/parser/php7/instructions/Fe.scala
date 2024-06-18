package io.joern.bytecode.parser.php7.instructions

import fastparse.NoWhitespace._
import fastparse._
import io.joern.bytecode.parser.constructs._
import io.joern.bytecode.parser.php7.Literals.getArray
import io.joern.bytecode.parser.php7.Variables.getAnyVariable
import io.joern.bytecode.parser.php7.instructions.Utility.{
  getAnyValue,
  parseTarget
}

object Fe {

  def parseFeFetchR[_: P]: P[(Variable, Variable, String)] =
    P("FE_FETCH_R " ~/ getAnyVariable ~ " " ~/ getAnyVariable ~ " " ~/ parseTarget)
  def getFeFetchR[_: P]: P[TripleValueOperation] =
    P(
      parseFeFetchR.map(
        x =>
          TripleValueOperation("FE_FETCH_R",
                               x._1,
                               x._2,
                               IntegerLiteral(x._3.toLong))))

  def parseFeFetchRw[_: P]: P[(Variable, Variable, String)] =
    P("FE_FETCH_RW " ~/ getAnyVariable ~ " " ~/ getAnyVariable ~ " " ~/ parseTarget)
  def getFeFetchRw[_: P]: P[TripleValueOperation] =
    P(
      parseFeFetchRw.map(
        x =>
          TripleValueOperation("FE_FETCH_RW",
                               x._1,
                               x._2,
                               IntegerLiteral(x._3.toLong))))

  def getFeFetch[_: P]: P[Opcode] =
    P(
      &("FE_FETCH_RW ") ~/ getFeFetchRw |
        &("FE_FETCH_R ") ~/ getFeFetchR)

  def parseFeResetRw[_: P]: P[(Value, String)] =
    P("FE_RESET_RW " ~/ (getAnyVariable | getArray) ~ " " ~ parseTarget)
  def getFeResetRw[_: P]: P[DualValueOperation] =
    P(parseFeResetRw.map(x =>
      DualValueOperation("FE_RESET_RW", x._1, IntegerLiteral(x._2.toLong))))

  def parseFeResetR[_: P]: P[(Value, String)] =
    P("FE_RESET_R " ~/ getAnyValue ~ " " ~/ parseTarget)
  def getFeResetR[_: P]: P[DualValueOperation] =
    P(parseFeResetR.map(x =>
      DualValueOperation("FE_RESET_R", x._1, IntegerLiteral(x._2.toLong))))

  def getFeReset[_: P]: P[Opcode] =
    P(
      &("FE_RESET_RW ") ~ getFeResetRw |
        &("FE_RESET_R ") ~ getFeResetR
    )

}
