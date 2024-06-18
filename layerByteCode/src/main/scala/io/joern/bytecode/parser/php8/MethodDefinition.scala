package io.joern.bytecode.parser.php8

import fastparse.NoWhitespace._
import fastparse._
import io.joern.bytecode.parser.constructs.{
  ByteCodeDefinitionsBlock,
  ControlFlowDefinitionsBlock,
  MethodDefinitionPair
}
import io.joern.bytecode.parser.php8.ByteCodeBlock.getByteCodeBlock
import io.joern.bytecode.parser.php8.ControlFlowBlock.getControlFlowBlock

object MethodDefinition {

  def parseFullMethodDefinitionBlock[_: P]
    : P[(ByteCodeDefinitionsBlock, ControlFlowDefinitionsBlock)] =
    P(getByteCodeBlock ~/ "\n".? ~/ getControlFlowBlock)
  def getFullMethodDefinitionBlock[_: P]: P[MethodDefinitionPair] =
    P(
      parseFullMethodDefinitionBlock.map(
        x => MethodDefinitionPair(x._1, x._2)
      ))

}
