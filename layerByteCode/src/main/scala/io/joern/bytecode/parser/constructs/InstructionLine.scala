package io.joern.bytecode.parser.constructs

//import ByteCodeParser._
sealed trait ByteCodeDumpLine

case class ExceptionTableLine(content: List[String]) extends ByteCodeDumpLine {}

case class LiveRangeLine(varNum: Int, start: Int, end: Int, rangeType: String)
    extends ByteCodeDumpLine

case class MethodHeader(var name: String,
                        var classname: Option[String],
                        var namespace: Option[String],
                        lines: Int,
                        args: Int,
                        vars: Int,
                        tmps: Int)
    extends ByteCodeDumpLine {
  // PHP treats class names and namespace names case insensitive
  classname match {
    case Some(name) => classname = Some(name.toLowerCase)
    case None       =>
  }
  namespace match {
    case Some(name) => namespace = Some(name.toLowerCase)
    case None       =>
  }
  //this is required as the path traversals for the cpg use regexp and $ is a regexp special character
  if (name == "$_main") {
    name = "DLR_main"
  } else {
    assert(name != "DLR_main")
  }
  name = name.toLowerCase
}

case class MethodHeaderMetaParsingInfo(metaInfo: Seq[String])
    extends ByteCodeDumpLine

case class MethodHeaderMetaFileInfo(fileName: String,
                                    lineStart: Int,
                                    lineEnd: Int)
    extends ByteCodeDumpLine

object MethodHeaderMetaFileInfo {

  def apply(line: String): MethodHeaderMetaFileInfo = {
    val split: Array[String] = line.split(":")
    val (lstart, lend) = split.last.split("-").toList match {
      case start :: end :: Nil =>
        (start.toInt, end.toInt)
      case x =>
        throw new RuntimeException(s"bad method header meta file info $x")
    }
    val file = split.slice(0, split.length - 1).mkString(":")
    MethodHeaderMetaFileInfo(file, lstart, lend)
  }

}

case class MethodHeaderRangeLine(string: String) extends ByteCodeDumpLine

case class BBDefinitionLine(number: Int,
                            attributes: Seq[String],
                            firstInstruction: Int,
                            lastInstruction: Int)
    extends ByteCodeDumpLine

sealed trait Instruction

case class InstructionLine(opNumber: Option[Integer],
                           var fileLine: Option[Integer],
                           instruction: Instruction)
    extends ByteCodeDumpLine

case class Assignment(lhs: Variable, rhs: Opcode) extends Instruction {
  override def toString: String = "ASSIGN"
}

case class Operation(op: Opcode) extends Instruction {
  override def toString: String = op.code
}
