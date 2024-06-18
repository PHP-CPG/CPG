package io.joern.bytecode.passes

import io.joern.reporting.Reporting
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, nodes}
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool, ParallelCpgPass}
import io.shiftleft.semanticcpg.language._

import scala.jdk.CollectionConverters._

class DeleteEmptyMethodDeclarations(methods: Seq[nodes.Method],
                                    cpg: Cpg,
                                    keyPool: IntervalKeyPool)
    extends ParallelCpgPass[nodes.Method](cpg,
                                          keyPools =
                                            Some(keyPool.split(methods.size)))
    with Reporting {

  override val name = "DeleteEmptyMethodDeclaration"
  override def partIterator: Iterator[nodes.Method] = methods.iterator

  override def runOnPart(part: Method): Iterator[DiffGraph] = {
    implicit val diffGraph: DiffGraph.Builder = DiffGraph.newBuilder
    withErrorReporting(part) {
      // check whether the corresponding code block is empty besides the always existing return
      if (part.astChildren.isBlock.astChildren.l.length == 1) {
        //if so iterate over all ast children of the method
        part.outE(EdgeTypes.AST).asScala.foreach { edgeToChild =>
          diffGraph.removeEdge(edgeToChild)
          val childNode = edgeToChild.inNode().asInstanceOf[nodes.StoredNode]
          diffGraph.removeNode(childNode)
        }
        // finally delete the method
        diffGraph.removeNode(part)
      }
    }
    Iterator(diffGraph.build())
  }
}

object DeleteEmptyMethodDeclarations {

  def getMethodDeclarations(cpg: Cpg): Seq[nodes.Method] = {
    cpg.method.l
  }

}
