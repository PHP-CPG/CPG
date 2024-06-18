package io.joern.bytecode.parser.php7

import fastparse.NoWhitespace._
import fastparse._
import io.joern.bytecode.parser.constructs.{ExceptionTable, ExceptionTableLine}
import io.joern.bytecode.parser.php7.instructions.Utility.parseTarget

object ExceptionTableBlock {

  def parseExceptionTableLine[_: P]: P[Seq[String]] =
    P(
      " ".rep
        ~ ((parseTarget | "-".!) ~ ", ").rep(1)
        ~ (parseTarget | "-".!)
    ).map(x => x._1 :+ x._2)

  def getExceptionTableLine[_: P]: P[ExceptionTableLine] =
    P(parseExceptionTableLine.map { x =>
      ExceptionTableLine(x.toList)
    })

  def parseExceptionTableBlock[_: P]: P[Seq[ExceptionTableLine]] =
    P("EXCEPTION TABLE:\n" ~ (getExceptionTableLine ~ "\n").rep)

  def getExceptionTableBlock[_: P]: P[ExceptionTable] =
    P(parseExceptionTableBlock.map(x => ExceptionTable(x)))

}
