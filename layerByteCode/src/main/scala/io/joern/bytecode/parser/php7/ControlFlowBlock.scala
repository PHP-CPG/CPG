package io.joern.bytecode.parser.php7

import fastparse.NoWhitespace._
import fastparse._
import io.joern.bytecode.parser.constructs._
import io.joern.bytecode.parser.php7.Basics._
import io.joern.bytecode.parser.php7.ExceptionTableBlock.getExceptionTableBlock
import io.joern.bytecode.parser.php7.HeaderBlock._
import io.joern.bytecode.parser.php7.Instructions._
import io.joern.bytecode.parser.php7.LiveRangesBlock.getLiveRangesBlock

object ControlFlowBlock {

  def parseBBInstructionLine[_: P]: P[Instruction] = P(" ".rep ~ getInstruction)
  def getBBInstructionLine[_: P]: P[InstructionLine] =
    P(parseBBInstructionLine.map(x => InstructionLine(None, None, x)))

  def parseBBIdent[_: P]: P[String] = P("BB" ~ anyNumber.rep.!)
  def getBBIdent[_: P]: P[Int] = P(parseBBIdent.map(Integer.parseInt))

  def parseBBAttribute[_: P]: P[Unit] =
    P("start" | "exit" | "target" | "follow" | "unreachable_free" | "unreachable" | "catch" | "try" | "finally_end" | "finally")

  def parseBBInstructionLines[_: P]: P[(String, String)] =
    P("lines=[" ~ anyNumber.rep.! ~ "-" ~ anyNumber.rep.! ~ "]")
  def getBBInstructionLines[_: P]: P[(Int, Int)] =
    P(parseBBInstructionLines.map(x =>
      (Integer.parseInt(x._1), Integer.parseInt(x._2))))

  def parseBBDefinitionLine[_: P]: P[(Int, Seq[String], (Int, Int))] =
    P(getBBIdent ~ ": " ~ (parseBBAttribute.! ~ " ").rep ~ getBBInstructionLines)
  def getBBDefinitionLine[_: P]: P[BBDefinitionLine] =
    P(parseBBDefinitionLine.map(x =>
      BBDefinitionLine(x._1, x._2, x._3._1, x._3._2)))

  def parseBBToLine[_: P]: P[Seq[String]] =
    P(" ".rep ~ ";" ~ " to=(" ~ ("BB" ~ anyNumber.rep.! ~ ", ".?).rep ~ ")")
  def getBBToLine[_: P]: P[Seq[Int]] =
    P(parseBBToLine.map(x => x.map(Integer.parseInt)))

  def parseBasicBlock[_: P]
    : P[(BBDefinitionLine, Option[Seq[Int]], Seq[InstructionLine])] =
    P(getBBDefinitionLine ~ ("\n" ~ getBBToLine).? ~ "\n" ~ (getBBInstructionLine ~ "\n").rep)
  def getBasicBlock[_: P]: P[BasicBlock] =
    P(
      parseBasicBlock.map(
        x =>
          BasicBlock(x._1.number,
                     x._1.attributes,
                     x._1.firstInstruction,
                     x._1.lastInstruction,
                     x._3,
                     x._2)))

  def parseControlFlowBlock[_: P]: P[(MethodHeader,
                                      MethodHeaderMetaParsingInfo,
                                      MethodHeaderMetaFileInfo,
                                      Seq[BasicBlock],
                                      Option[LiveRanges],
                                      Option[ExceptionTable])] =
    P(
      getHeaderBlock ~ getBasicBlock.rep ~
        (&("LIVE RANGES:") ~ getLiveRangesBlock).? ~
        (&("EXCEPTION TABLE:") ~ getExceptionTableBlock).?)
  def getControlFlowBlock[_: P]: P[ControlFlowDefinitionsBlock] =
    P(
      parseControlFlowBlock.map(x =>
        ControlFlowDefinitionsBlock(
          x._1.name,
          x._1.classname,
          x._1.namespace,
          x._1.lines,
          x._1.args,
          x._1.vars,
          x._1.tmps,
          x._2.metaInfo,
          x._3.fileName,
          x._3.lineStart,
          x._3.lineEnd,
          None, // php7 has no range line
          x._4,
          x._5,
          x._6
      )))

}
