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
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, nodes}
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool, ParallelCpgPass}
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.NodeOps


class CfgIntraBBCreationPass(
    filesMethodDefinitionPairs: List[Seq[MethodDefinitionPair]],
    cpg: Cpg,
    keyPool: IntervalKeyPool)
    extends ParallelCpgPass[Seq[MethodDefinitionPair]](
      cpg,
      keyPools = Some(keyPool.split(filesMethodDefinitionPairs.size)))
    with Reporting {

  //println(filesMethodDefinitionPairs.map(_.map(_.byteCodeBlock)))

  override def partIterator: Iterator[Seq[MethodDefinitionPair]] =
    filesMethodDefinitionPairs
      .map(_.filterNot(_.byteCodeBlock.name == "{closure}"))
      .iterator
  implicit val codePropertyGraph: Cpg = cpg
  override val name = "CfgIntraBBCreationPass"

  override def runOnPart(fileMethodDefinitionPairs: Seq[MethodDefinitionPair])
    : Iterator[DiffGraph] = {
    implicit val diffGraph: DiffGraph.Builder = DiffGraph.newBuilder
    //getting the absolute path to the file
    val absolutePath = MethodIdentification.getAbsolutePath(
      fileMethodDefinitionPairs.head.byteCodeBlock.fileName)

    fileMethodDefinitionPairs.foreach { cfgBlock =>
      try {
        cfgBlock match {
          case MethodDefinitionPair(_, cfgBlock) =>
            val method = getMethod(absolutePath, cfgBlock)
            createControlFlow(cfgBlock, method)
        }
      } catch {
        case x: ReportableError =>
          reportError(x)
        case x: Throwable =>
          reportError(absolutePath,
                      cfgBlock.byteCodeBlock.lineStart.toString,
                      cfgBlock.byteCodeBlock.name,
                      "",
                      x.getMessage,
                      x)
      }
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

    val methodBodyCalls: Array[nodes.Call] =
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
                                "the method does not have a code block")
      }

    // creating the in BB control flow
    val basicBlocks =
      cfgBlock.blocks.sortWith((lhs, rhs) => lhs.number < rhs.number).toArray

    basicBlocks.foreach { basicBlock =>
      // create the control flow within the basic block
      createControlFlowIntraBasicBlock(basicBlock, methodBodyCalls, diffGraph)
    }
  }

  def createControlFlowIntraBasicBlock(basicBlock: BasicBlock,
                                       bodyBlockCalls: Array[nodes.Call],
                                       graph: DiffGraph.Builder): Unit = {
    implicit val diffGraph: DiffGraph.Builder = graph
    var previous = createCfgForCall(bodyBlockCalls(basicBlock.firstInstruction))
    //go through the given instruction range and connect each instruction in the BB with a CFG edge
    for (instruction <- Range.inclusive(basicBlock.firstInstruction + 1,
                                        basicBlock.lastInstruction)) {
      val next = createCfgForCall(bodyBlockCalls(instruction))
      addCfgEdge(previous._2, next._1) //, List(("condition","always")))
      previous = next
    }
  }

  private def createCfgForCall(call: nodes.Call)(
      implicit diffGraph: DiffGraph.Builder): (nodes.CfgNode, nodes.CfgNode) = {
    // check if we are withing an =  assignment
    if (call.name == "=") {
      // if so we need to recurse on the rhs call
      val (start, end) = createCfgForCall(
        call.astChildren.order(1).head.asInstanceOf[nodes.Call])
      // add an edge from the end of the rhs call to the lhs of the assignment
      addCfgEdge(end,
                 call.astChildren.order(0).head.asInstanceOf[nodes.CfgNode])
      // add an edge from the lhs of the assignment to the current call
      addCfgEdge(call.astChildren.order(0).head.asInstanceOf[nodes.CfgNode],
                 call)
      // return the start of the rhs call as the entry to this block and the call itself as the exit
      (start, call)
    } else {
      // we are not within an = assignment
      val arguments =
        call.start.argument.l.sortWith((lhs, rhs) => lhs.order < rhs.order)
      // match on the amount of arguments the call has
      arguments match {
        // if there is no argument
        case Nil =>
          // return the call itself as both start and exit
          (call, call)
        case x :: Nil =>
          // if there is a single argument add an edge from the argument to the call
          addCfgEdge(x, call)
          // return the argument as the entry and the call as exit
          (x, call)
        case _ =>
          // if we have more than one argument
          arguments.sliding(2).foreach {
            case x :: y :: Nil => addCfgEdge(x, y)
            case _ =>
              throw ReportableError("", -1, "", call.code, "unexpected")
          }
          addCfgEdge(arguments.last, call)
          (arguments.head, call)
      }
    }
  }

}
