package io.joern.bytecode.parser.constructs

case class ExceptionTable(tableEntry: Seq[ExceptionTableLine])

case class LiveRanges(rangesEntry: Seq[LiveRangeLine])

abstract class MethodDefinition(name: String,
                                classname: Option[String],
                                namespace: Option[String]) {

  val lines: Int
  val args: Int
  val vars: Int
  val tmps: Int
  val parsingMetaInfo: Seq[String]
  val fileName: String
  val lineStart: Int
  val lineEnd: Int

  def fullyQualifiedName: String = {
    val nspace = namespace match {
      case Some(name) => name + "\\"
      case None       => ""
    }
    val cname = classname match {
      case Some(name) => name + "::"
      case None       => ""
    }
    s"$nspace$cname$name"
  }
}

case class ByteCodeDefinitionsBlock(name: String,
                                    classname: Option[String],
                                    namespace: Option[String],
                                    lines: Int,
                                    args: Int,
                                    vars: Int,
                                    tmps: Int,
                                    parsingMetaInfo: Seq[String],
                                    fileName: String,
                                    lineStart: Int,
                                    lineEnd: Int,
                                    rangeLine: Option[String],
                                    instructions: Seq[InstructionLine],
                                    liveRanges: Option[LiveRanges],
                                    exceptionTable: Option[ExceptionTable])
    extends MethodDefinition(name, classname, namespace)

case class BasicBlock(number: Int,
                      attributes: Seq[String],
                      firstInstruction: Int,
                      lastInstruction: Int,
                      instructions: Seq[InstructionLine],
                      followedBy: Option[Seq[Int]])

case class ControlFlowDefinitionsBlock(name: String,
                                       classname: Option[String],
                                       namespace: Option[String],
                                       lines: Int,
                                       args: Int,
                                       vars: Int,
                                       tmps: Int,
                                       parsingMetaInfo: Seq[String],
                                       fileName: String,
                                       lineStart: Int,
                                       lineEnd: Int,
                                       rangeLine: Option[String],
                                       blocks: Seq[BasicBlock],
                                       liveRanges: Option[LiveRanges],
                                       exceptionTable: Option[ExceptionTable])
    extends MethodDefinition(name, classname, namespace)

case class MethodDefinitionPair(byteCodeBlock: ByteCodeDefinitionsBlock,
                                controlFlowBlock: ControlFlowDefinitionsBlock)
