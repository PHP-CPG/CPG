package io.joern.bytecode.passes

import io.joern.bytecode.Defines
import io.joern.bytecode.parser.constructs.{
  BasicBlock,
  ControlFlowDefinitionsBlock,
  MethodDefinitionPair
}
import io.joern.bytecode.passes.utility.MethodIdentification
import io.joern.reporting.{ReportableError, Reporting}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes, nodes}
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool, ParallelCpgPass}
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.NodeOps

class CfgInterBBCreationPass(
    filesMethodDefinitionPairs: List[Seq[MethodDefinitionPair]],
    cpg: Cpg,
    keyPool: IntervalKeyPool)
    extends ParallelCpgPass[Seq[MethodDefinitionPair]](
      cpg,
      keyPools = Some(keyPool.split(filesMethodDefinitionPairs.size)))
    with Reporting {

  override val name = "CfgInterBBCreationPass"
  implicit val codePropertyGraph: Cpg = cpg

  override def partIterator: Iterator[Seq[MethodDefinitionPair]] =
    filesMethodDefinitionPairs
      .map(_.filterNot(_.byteCodeBlock.name == "{closure}"))
      .iterator

  override def runOnPart(fileMethodDefinitionPairs: Seq[MethodDefinitionPair])
    : Iterator[DiffGraph] = {
    implicit val diffGraph: DiffGraph.Builder = DiffGraph.newBuilder
    try {
      val absolutePath = MethodIdentification.getAbsolutePath(
        fileMethodDefinitionPairs.head.byteCodeBlock.fileName)

      val methodDefinitionPairs: Seq[MethodDefinitionPair] =
        fileMethodDefinitionPairs

      methodDefinitionPairs.foreach {
        case MethodDefinitionPair(_, cfgBlock) =>
          val method = getMethod(absolutePath, cfgBlock)
          createControlFlow(cfgBlock, method)
      }
    } catch {
      case x: ReportableError =>
        reportError(x)
      case x: Throwable =>
        reportError(fileMethodDefinitionPairs.head.byteCodeBlock.fileName,
                    "",
                    "",
                    "",
                    x.getMessage,
                    x)
    }
    Iterator(diffGraph.build())
  }

  private def getMethod(fileName: String,
                        cfgBlock: ControlFlowDefinitionsBlock): nodes.Method = {
    MethodIdentification.getUniqueMethod(
      Some(fileName),
      cfgBlock.namespace match {
        case Some(_) => cfgBlock.namespace
        case None    => Some(Defines.GLOBAL_NAMESPACE_NAME)
      },
      cfgBlock.classname,
      cfgBlock.name,
      Some(cfgBlock.lineStart),
      Some(cfgBlock.lineEnd)
    )
  }

  def addCfgEdge(start: nodes.CfgNode, end: nodes.CfgNode)(
      implicit diffGraph: DiffGraph.Builder): Unit = {
    diffGraph.addEdge(start, end, EdgeTypes.CFG)
  }

  def createControlFlow(
      cfgBlock: ControlFlowDefinitionsBlock,
      method: nodes.Method)(implicit diffGraph: DiffGraph.Builder): Unit = {
    val methodBodyCalls =
      method.astChildren.isBlock.headOption
        .map { node =>
          node.astChildren.isCall.l
            .sortWith((a, b) => a.order < b.order)
            .toArray
        } match {
        case Some(value) => value
        case None =>
          throw ReportableError(cfgBlock.fileName,
                                cfgBlock.lineStart,
                                cfgBlock.name,
                                "",
                                s"The method does not have a code block")
      }
    // creating the in BB control flow
    val basicBlocks =
      cfgBlock.blocks.sortWith((lhs, rhs) => lhs.number < rhs.number).toArray

    basicBlocks.foreach { basicBlock =>
      // handle if the basic block is an entry block
      createControlFlowInterBasicBlock(basicBlock,
                                       basicBlocks,
                                       methodBodyCalls,
                                       method,
                                       diffGraph)
    }
  }

  def getEntryOfCall(call: nodes.Call): nodes.CfgNode = {
    // determine if we have an = assignment
    if (call.name == "=") {
      // if so the entry to this opcode is the entry of the rhs opcode
      getEntryOfCall(call.astChildren.order(1).head.asInstanceOf[nodes.Call])
    } else {
      // if not so
      // and the opcode does not have any arguments
      if (call.start.argument.l.isEmpty) {
        // then the call itself is the entry
        call
      } else {
        // else the first argument is the entry
        call.start.astChildren.order(0).head.asInstanceOf[nodes.CfgNode]
      }
    }
  }

  def createControlFlowInterBasicBlock(basicBlock: BasicBlock,
                                       basicBlocks: Array[BasicBlock],
                                       methodBodyCalls: Array[nodes.Call],
                                       method: nodes.Method,
                                       graph: DiffGraph.Builder): Unit = {
    basicBlock.attributes.find(attr => attr == "start") match {
      case Some(_) =>
        val block = method.astChildren.isBlock.l
        assert(block.length == 1, "any method should only have one block child")
        graph.addEdge(method, block.head, EdgeTypes.CFG)
        graph.addEdge(
          block.head,
          getEntryOfCall(methodBodyCalls(basicBlock.firstInstruction)),
          EdgeTypes.CFG)
      case None =>
    }
    // handle if the basic block is an exit block
    basicBlock.attributes.find(attr => attr == "exit") match {
      case Some(_) =>
        graph.addEdge(methodBodyCalls(basicBlock.lastInstruction),
                      method.astChildren
                        .find(child => child.label == NodeTypes.METHOD_RETURN)
                        .get,
                      EdgeTypes.CFG)
      case None =>
    }
    basicBlock.followedBy match {
      case Some(followers) =>
        val previousInstruction = methodBodyCalls(basicBlock.lastInstruction)
        if (followers.length > 1) {
          followers.foreach { follower =>
            assert(
              follower < basicBlocks.length,
              s"jumping to BB$follower impossible as there are only ${basicBlocks.length} BBs")
            assert(
              basicBlocks(follower).firstInstruction < methodBodyCalls.length,
              s"selecting instruction ${basicBlocks(follower).firstInstruction} impossible as there are only ${methodBodyCalls.length} instructions"
            )
            val targetInstruction = getEntryOfCall(
              methodBodyCalls(basicBlocks(follower).firstInstruction))
            addCfgEdge(previousInstruction, targetInstruction)(graph)
          }
        } else {
          val List(first) = followers
          val followUpInstruction = getEntryOfCall(
            methodBodyCalls(basicBlocks(first).firstInstruction))
          addCfgEdge(previousInstruction, followUpInstruction)(graph)
        }
      case None =>
    }
  }

}
