package io.joern.bytecode.passes

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, nodes}
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool, ParallelCpgPass}
import io.shiftleft.semanticcpg.language._

import scala.jdk.CollectionConverters._

class OpDataPass(methods: Seq[nodes.Method], cpg: Cpg, keyPool: IntervalKeyPool)
    extends ParallelCpgPass[nodes.Method](cpg: Cpg,
                                          keyPools =
                                            Some(keyPool.split(methods.size))) {

  val knownOpDataUser =
    List("ASSIGN_OBJ",
         "ASSIGN_OBJ_OP",
         "ASSIGN_OBJ_REF",
         "ASSIGN_DIM",
         "ASSIGN_DIM_OP")

  override def partIterator: Iterator[Method] = methods.iterator

  override def runOnPart(part: Method): Iterator[DiffGraph] = {
    implicit val diffGraph: DiffGraph.Builder = DiffGraph.newBuilder
    val opDataNodes = part.astMinusRoot.isCall.nameExact("OP_DATA")
    opDataNodes.foreach { opDataNode =>
      handleOpDataCall(opDataNode)
    }
    Iterator(diffGraph.build())
  }

  def getCorrespondingPreviousOperation(
      call: nodes.CfgNode): Option[nodes.CfgNode] = {
    if (call.isCall && knownOpDataUser.contains(
          call.asInstanceOf[nodes.Call].name)) {
      Some(call)
    } else {
      val previous = call._cfgIn.asScala.toList.asInstanceOf[nodes.CfgNode]
      previous.foldLeft(None: Option[nodes.CfgNode]) {
        case (Some(prev), _) => Some(prev)
        case (None, current) => getCorrespondingPreviousOperation(current)
      }
    }
  }

  def handleOpDataCall(opData: nodes.Call)(
      implicit graph: DiffGraph.Builder): Unit = {
    assert(opData.astChildren.order(0).l.length == 1)
    val argument = opData.astChildren.order(0).head
    val correspondingOpCall = getCorrespondingPreviousOperation(opData)
      .getOrElse(
        throw new RuntimeException(
          s"Did not find previous operation for OP_DATA ${opData.toString}"))
    graph.addEdge(correspondingOpCall, argument, EdgeTypes.ARGUMENT)
  }
}
