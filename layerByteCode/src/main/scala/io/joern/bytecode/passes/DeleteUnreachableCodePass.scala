package io.joern.bytecode.passes

import io.joern.reporting.{ReportableError, Reporting}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, Call, Method}
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool, ParallelCpgPass}
import io.shiftleft.semanticcpg.language._
import overflowdb.Edge

import java.util.function._
import scala.jdk.CollectionConverters._

class DeleteUnreachableCodePass(methods: Seq[nodes.Method],
                                cpg: Cpg,
                                keyPool: IntervalKeyPool)
    extends ParallelCpgPass[nodes.Method](cpg,
                                          keyPools =
                                            Some(keyPool.split(methods.size)))
    with Reporting {

  override val name = "DeleteUnreachableCodePass"

  override def partIterator: Iterator[Method] = methods.iterator

  override def runOnPart(part: Method): Iterator[DiffGraph] = {
    implicit val diffGraph: DiffGraph.Builder = DiffGraph.newBuilder
    val edgesToBeRemoved = collection.mutable.Set[Edge]()
    val callsToBeRemoved = collection.mutable.Set[Call]()

    val markEdgeForDeletionConsumer: Consumer[Edge] = (t: Edge) =>
      edgesToBeRemoved.add(t)
    try {
      part.astMinusRoot.isCall
        .filter { call =>
          //unless we have dead code there is always an incoming edge and the outgoing edge to the RETURN node
          //or a edge to the next instruction in the BB
          call._cfgIn.asScala.toList.isEmpty && call._cfgOut.asScala.toList.isEmpty && call.code != "VERIFY_NEVER_TYPE"
        }
        .foreach { deadCall: Call =>
          callsToBeRemoved.add(deadCall)
          //remove all possible outgoing edges (this can happen, i.e., with a  dead jump)
          deadCall.outE().forEachRemaining(markEdgeForDeletionConsumer)
          //remove all incoming edges (well it is still part of the AST and we do not want dangling edges)
          deadCall.inE().forEachRemaining(markEdgeForDeletionConsumer)
        }
      edgesToBeRemoved.toList.foreach(diffGraph.removeEdge)
      callsToBeRemoved.toList.foreach(diffGraph.removeNode(_))
    } catch {
      case x: ReportableError => reportError(x)
      case x: Throwable =>
        reportError(part.filename, "", part.fullName, "", x.getMessage, x)
    }
    Iterator(diffGraph.build())
  }

  def removeNode(node: AstNode,
                 diff: DiffGraph.Builder,
                 edgeRemover: Consumer[Edge]): Unit = {
    diff.removeNode(node)
    node.outE().forEachRemaining(edgeRemover)
    node.inE().forEachRemaining(edgeRemover)
  }

}
