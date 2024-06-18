package io.joern.bytecode.parser.php7

import fastparse.NoWhitespace._
import fastparse._
import io.joern.bytecode.parser.constructs._
import io.joern.bytecode.parser.php7.Basics._
import io.joern.bytecode.parser.php7.ExceptionTableBlock._
import io.joern.bytecode.parser.php7.HeaderBlock._
import io.joern.bytecode.parser.php7.Instructions._
import io.joern.bytecode.parser.php7.LiveRangesBlock._

object ByteCodeBlock {

  def parseLineNumber[_: P]: P[String] = P("L" ~ anyNumber.rep.!)
  def getLineNumber[_: P]: P[Integer] =
    P(parseLineNumber.map(x => Integer.valueOf(x)))

  def parseFileLineNumber[_: P]: P[String] = P("(" ~ anyNumber.rep.! ~ ")")
  def getFileLineNUmber[_: P]: P[Integer] =
    P(parseFileLineNumber.map(x => Integer.valueOf(x)))

  def parseDefiningInstructionLine[_: P]: P[(Integer, Integer, Instruction)] =
    P(getLineNumber ~ " " ~ getFileLineNUmber ~ ":" ~/ " ".rep ~ getInstruction)
  def getDefiningInstructionLine[_: P]: P[InstructionLine] =
    P(parseDefiningInstructionLine.map(x =>
      InstructionLine(Some(x._1), Some(x._2), x._3)))

  def parseByteCodeBlock[_: P]: P[(MethodHeader,
                                   MethodHeaderMetaParsingInfo,
                                   MethodHeaderMetaFileInfo,
                                   Seq[InstructionLine],
                                   Option[LiveRanges],
                                   Option[ExceptionTable])] =
    P(
      getHeaderBlock ~
        (&("L" ~ anyNumber) ~ getDefiningInstructionLine ~/ "\n").rep ~
        (&("LIVE RANGES:") ~ getLiveRangesBlock).? ~
        (&("EXCEPTION TABLE:") ~ getExceptionTableBlock).?)
  def getByteCodeBlock[_: P]: P[ByteCodeDefinitionsBlock] =
    P(
      parseByteCodeBlock.map(x =>
        ByteCodeDefinitionsBlock(
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
