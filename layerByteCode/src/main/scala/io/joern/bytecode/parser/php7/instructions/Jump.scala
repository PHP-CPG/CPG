package io.joern.bytecode.parser.php7.instructions

import fastparse.NoWhitespace._
import fastparse._
import io.joern.bytecode.parser.constructs._
import io.joern.bytecode.parser.php7.Basics.anyNumber
import io.joern.bytecode.parser.php7.instructions.Utility.getAnyValue

object Jump {

  def parseJmpnz[_: P]: P[(Value, String)] =
    P("JMPNZ " ~ getAnyValue ~ " " ~ Utility.parseTarget)
  def getJmpnz[_: P]: P[DualValueOperation] =
    P(parseJmpnz.map(x =>
      DualValueOperation("JMPNZ", x._1, IntegerLiteral(x._2.toLong))))

  def parseJmpnzEx[_: P]: P[(Value, String)] =
    P("JMPNZ_EX " ~ getAnyValue ~ " " ~ ("L" | "BB") ~ anyNumber.rep.!)
  def getJmpnzEx[_: P]: P[DualValueOperation] =
    P(parseJmpnzEx.map(x =>
      DualValueOperation("JMPNZ_EX", x._1, IntegerLiteral(x._2.toLong))))

  def parseJmpz[_: P]: P[(Value, String)] =
    P("JMPZ " ~ getAnyValue ~ " " ~ Utility.parseTarget)
  def getJmpz[_: P]: P[DualValueOperation] =
    P(parseJmpz.map(x =>
      DualValueOperation("JMPZ", x._1, IntegerLiteral(x._2.toLong))))

  def parseJmp[_: P]: P[String] = P("JMP " ~ Utility.parseTarget)
  def getJmp[_: P]: P[SingleValueOperation] =
    P(parseJmp.map(x => SingleValueOperation("JMP", IntegerLiteral(x.toLong))))

  def parseJmpzEx[_: P]: P[(Value, String)] =
    P("JMPZ_EX " ~ getAnyValue ~ " " ~ ("L" | "BB") ~ anyNumber.rep.!)
  def getJmpzEx[_: P]: P[DualValueOperation] =
    P(parseJmpzEx.map(x =>
      DualValueOperation("JMPZ_EX", x._1, IntegerLiteral(x._2.toLong))))

  def parseJmpZnz[_: P]: P[(Value, String, String)] = P(
    "JMPZNZ " ~ getAnyValue ~ " " ~
      Utility.parseTarget ~ " " ~
      Utility.parseTarget
  )
  def getJmpZnz[_: P]: P[TripleValueOperation] =
    P(
      parseJmpZnz.map(
        x =>
          TripleValueOperation("JMPZNZ",
                               x._1,
                               IntegerLiteral(x._2.toLong),
                               IntegerLiteral(x._3.toLong))
      ))

  def parseJmpSet[_: P]: P[(Value, String)] =
    P("JMP_SET" ~ " " ~ getAnyValue ~ " " ~ Utility.parseTarget)
  def getJmpSet[_: P]: P[DualValueOperation] =
    P(parseJmpSet.map(x =>
      DualValueOperation("JMP_SET", x._1, IntegerLiteral(x._2.toLong))))

  def parseJmpNull[_: P]: P[(Value, String)] =
    P("JMP_NULL " ~ getAnyValue ~ " " ~ Utility.parseTarget)

  def getJmpNull[_: P]: P[DualValueOperation] =
    P(parseJmpNull.map(x =>
      DualValueOperation("JMP_NULL", x._1, IntegerLiteral(x._2.toLong))))

  def getJmpCommand[_: P]: P[Opcode] =
    P(
      &("JMPZ_EX ") ~ getJmpzEx |
        &("JMPNZ_EX ") ~ getJmpnzEx |
        &("JMPNZ ") ~ getJmpnz |
        &("JMPZ ") ~ getJmpz |
        &("JMPZNZ ") ~ getJmpZnz |
        &("JMP_SET ") ~ getJmpSet |
        &("JMP_NULL") ~ getJmpNull |
        &("JMP ") ~ getJmp
    )

}
