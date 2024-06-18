package io.joern.bytecode.parser.php8

import fastparse.NoWhitespace._
import fastparse._
import io.joern.bytecode.parser.constructs._
import io.joern.bytecode.parser.php8.Basics.anyNumber
import io.joern.bytecode.parser.php8.ExceptionTableBlock.getExceptionTableBlock
import io.joern.bytecode.parser.php8.HeaderBlock.getHeaderBlock
import io.joern.bytecode.parser.php8.Instructions.getInstruction
import io.joern.bytecode.parser.php8.LiveRangesBlock.getLiveRangesBlock

object ByteCodeBlock {

  def parseLineNumber[_: P]: P[String] = P(anyNumber.rep.!)
  def getLineNumber[_: P]: P[Integer] =
    P(parseLineNumber.map(x => Integer.valueOf(x)))

  def parseFileLineNumber[_: P]: P[String] = P("(" ~ anyNumber.rep.! ~ ")")
  def getFileLineNUmber[_: P]: P[Integer] =
    P(parseFileLineNumber.map(x => Integer.valueOf(x)))

  def parseDefiningInstructionLine[_: P]: P[(Integer, Instruction)] =
    P(getLineNumber ~/ " ".rep ~ getInstruction)
  def getDefiningInstructionLine[_: P]: P[InstructionLine] =
    P(parseDefiningInstructionLine.map(x =>
      InstructionLine(Some(x._1), None, x._2)))

  def parseByteCodeBlock[_: P]: P[(MethodHeader,
                                   MethodHeaderMetaParsingInfo,
                                   MethodHeaderMetaFileInfo,
                                   MethodHeaderRangeLine,
                                   Seq[InstructionLine],
                                   Option[LiveRanges],
                                   Option[ExceptionTable])] =
    P(
      getHeaderBlock ~
        (&(anyNumber) ~ getDefiningInstructionLine ~/ "\n").rep ~
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
          Some(x._4.string),
          x._5.map(it => {
            val tmp = it;
            tmp.fileLine = Some(x._3.lineStart);
            tmp
          }),
          x._6,
          x._7
      )))

}
