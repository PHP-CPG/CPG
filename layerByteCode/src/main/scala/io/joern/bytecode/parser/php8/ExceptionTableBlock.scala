package io.joern.bytecode.parser.php8

import fastparse.NoWhitespace._
import fastparse._
import io.joern.bytecode.parser.constructs.{ExceptionTable, ExceptionTableLine}
import io.joern.bytecode.parser.php8.instructions.Utility.parseTarget

object ExceptionTableBlock {

  def parseExceptionTableLine[_: P]: P[Seq[String]] =
    P(
      " ".rep ~ (" ".rep ~ (parseTarget | "-".!) ~ ", ".?).rep(1) ~ "\n"
    )

  def getExceptionTableLine[_: P]: P[ExceptionTableLine] =
    P(parseExceptionTableLine.map { x =>
      ExceptionTableLine(x.toList)
    })

  def parseExceptionTableBlock[_: P]: P[Seq[ExceptionTableLine]] =
    P("EXCEPTION TABLE:\n" ~/ getExceptionTableLine.rep(1))

  def getExceptionTableBlock[_: P]: P[ExceptionTable] =
    P(parseExceptionTableBlock.map(x => ExceptionTable(x)))

}
