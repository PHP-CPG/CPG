package io.joern.bytecode.parser.php8.instructions

import fastparse.NoWhitespace._
import fastparse._
import io.joern.bytecode.parser.constructs._
import io.joern.bytecode.parser.php8.instructions.Utility.getAnyValue

object Bind {

  def parseBindStatic[_: P]: P[Seq[Value]] =
    P(&("BIND_STATIC ") ~ "BIND_STATIC" ~ (" " ~ getAnyValue).rep)
  def getBindStatic[_: P]: P[Opcode] =
    P(parseBindStatic.map {
      case first :: second :: Nil =>
        DualValueOperation("BIND_STATIC", first, second)
      case first :: Nil => SingleValueOperation("BIND_STATIC", first)
    })

  def parseBindLexical[_: P]: P[Seq[Value]] =
    P(&("BIND_LEXICAL ") ~ "BIND_LEXICAL" ~ (" " ~ getAnyValue).rep)
  def getBindLexical[_: P]: P[Opcode] =
    P(parseBindLexical.map {
      case first :: second :: third :: Nil =>
        TripleValueOperation("BIND_LEXICAL", first, second, third)
      case first :: second :: Nil =>
        DualValueOperation("BIND_LEXICAL", first, second)
      case first :: Nil => SingleValueOperation("BIND_LEXICAL", first)
    })

}
