package io.joern.bytecode.passes

import io.joern.bytecode.Defines
import io.joern.bytecode.passes.utility.MethodDetectionAndAssociation._
import io.joern.reporting.Reporting
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, CfgNode, Method}
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, nodes}
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool, ParallelCpgPass}
import io.shiftleft.semanticcpg.language._

import scala.collection.mutable.{Set => MSet}
import scala.jdk.CollectionConverters._

class CallFinishingPass(methods: Seq[nodes.Method],
                        cpg: Cpg,
                        keyPool: IntervalKeyPool,
                        strict: Boolean = true)
    extends ParallelCpgPass[nodes.Method](cpg,
                                          keyPools =
                                            Some(keyPool.split(methods.size)))
    with Reporting {

  override val name: String = "CallFinishingPass"

  override def partIterator: Iterator[Method] = methods.iterator

  private def inMethodStructureOrder(node: Call): Int = {
    val order = if (node.astParent.isInstanceOf[Call]) {
      node.astParent.order
    } else {
      node.order
    }
    order
  }

  private def getInitCallsInMethodOrder(node: Method): List[Call] = {
    node.ast.isCall
      .filter(call => KNOWN_FUNCTION_STARTS.contains(call.name))
      .l
      .sortBy(inMethodStructureOrder)
  }

  override def runOnPart(part: Method): Iterator[DiffGraph] = {
    implicit val diffGraph: DiffGraph.Builder = DiffGraph.newBuilder
    withErrorReporting(part) {
      val remainingFunctionCallStarts = getInitCallsInMethodOrder(part)
      val finished: MSet[Call] = MSet()
      remainingFunctionCallStarts.foreach { call =>
        if (!finished.contains(call)) {
          processFunctionStart(call, List(), Map(), finished)
        }
      }
    }
    //println(s"${part.name}/${part.id()} -> " + diffGraph.build().iterator.toList)
    Iterator(diffGraph.build())
  }

  private def isCallRelated(cfgNode: CfgNode): Boolean = {
    cfgNode match {
      case x: Call =>
        KNOWN_FUNCTION_STARTS.contains(x.name) ||
          KNOWN_SEND_VALS.contains(x.name) ||
          KNOWN_FUNCTION_ENDS.contains(x.name)
      case _ => false
    }
  }

  private def processFunctionStart(
      current: CfgNode,
      activeInits: List[Call],
      activeSends: Map[Call, List[Call]],
      finished: MSet[Call])(implicit graph: DiffGraph.Builder): Unit = {
    //println(s"processing on ${current.code}")
    var actualActiveInits = activeInits
    var actualActiveSends = activeSends
    // if it is call related
    if (isCallRelated(current)) {
      // cast current to an actual call as it has to be
      val currentCall = current.asInstanceOf[Call]
      // if it is a known function start
      if (KNOWN_FUNCTION_STARTS.contains(currentCall.name)) {
        // push it on top of the active function calls
        actualActiveInits = List(currentCall) ++ actualActiveInits
      } else if (KNOWN_SEND_VALS.contains(currentCall.name)) {
        val activeInit: Call = actualActiveInits.head
        val activeSends: List[Call] =
          actualActiveSends.getOrElse(activeInit, List[Call]())
        actualActiveSends = actualActiveSends + (activeInit -> (activeSends ++ List[
          Call](currentCall)))
      } else {
        assert(KNOWN_FUNCTION_ENDS.contains(current.asInstanceOf[Call].name),
               s"there can only be a send_val left for $current")
        // pop head of active inits
        val activeInit: Call = actualActiveInits.head
        actualActiveInits = actualActiveInits.tail
        // get active related sends
        val activeSends: List[Call] =
          actualActiveSends.getOrElse(activeInit, List())
        // if we did not already handle the current
        if (!finished.contains(activeInit)) {
          // handle the call
          finishUpCall(activeInit, activeSends, currentCall)
          // add finished call to finished
          finished.addOne(activeInit)
        }
      }
    }
    // if there are still active calls
    if (actualActiveInits.nonEmpty) {
      // we are assuming no loops
      current.out(EdgeTypes.CFG).asScala.foreach { next =>
        processFunctionStart(next.asInstanceOf[CfgNode],
                             actualActiveInits,
                             actualActiveSends,
                             finished)
      }
    } else {
      // if not we are done
    }
  }

  private def finishUpCall(init: Call, sends: List[Call], docall: Call)(
      implicit graph: DiffGraph.Builder): Unit = {
    val nameOfCalledFunction = getCalledMethod(init)
    //val unknownNameOfCalledFunction = nameOfCalledFunction.replace("*","UNKNOWN")
    // given that we have different send vals (i.e., possibly with named parameter in PHP8) we are now linking to the SEND_VAL
    // and do not give an order
    sends.foreach { send =>
      graph.addEdge(docall, send, EdgeTypes.ARGUMENT)
    }
    graph.addNodeProperty(docall, "NAME", nameOfCalledFunction)
    getCallCorrespondingMethod(cpg, nameOfCalledFunction) match {
      case Nil =>
        getCallCorrespondingMethod(cpg, nameOfCalledFunction.split("\\\\").last) match {
          case single :: Nil if single.code == Defines.INTERNAL_FUNCTION =>
            graph.addEdge(docall, single, EdgeTypes.CALL)
          case _ =>
            throw new RuntimeException(
              s"Missing linking target for $nameOfCalledFunction. Either stub creation was not run or failed")
        }
      case single :: Nil =>
        graph.addEdge(docall, single, EdgeTypes.CALL)
      case multiple if strict =>
        val actualUnknownName = nameOfCalledFunction.replace(".*", "UNKNOWN")
        multiple.filter(
          x =>
            x.fullName == actualUnknownName && List(
              Defines.MULTIPLE_TARGETS,
              Defines.UNKNOWN_FUNCTION,
              Defines.UNKNOWN_METHOD).contains(x.code)) match {
          case Nil =>
            throw new RuntimeException(
              s"Multiple targets but no $actualUnknownName stub. Either stub creation was not run or failed. Available were ${multiple
                .map(elem => s"${elem.fullName}/${elem.code}")}")
          case target :: Nil =>
            graph.addEdge(docall, target, EdgeTypes.CALL)
          case _ =>
            throw new RuntimeException(
              s"Multiple targets but multiple $actualUnknownName stub. Stub creation failed")
        }
      case multiple if !strict =>
        multiple.foreach { target =>
          graph.addEdge(docall, target, EdgeTypes.CALL)
        }
    }
  }
}
