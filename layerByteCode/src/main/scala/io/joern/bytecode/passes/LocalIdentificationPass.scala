package io.joern.bytecode.passes

import io.joern.reporting.{ReportableError, Reporting}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, nodes}
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool, ParallelCpgPass}
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.Traversal

class LocalIdentificationPass(methods: Seq[nodes.Method],
                              cpg: Cpg,
                              keyPool: IntervalKeyPool)
    extends ParallelCpgPass[nodes.Method](cpg,
                                          keyPools =
                                            Some(keyPool.split(methods.size)))
    with Reporting {

  override val name = "LocalIdentificationPass"
  override def partIterator: Iterator[Method] = methods.iterator

  override def runOnPart(part: Method): Iterator[DiffGraph] = {
    val diffGraph: DiffGraph.Builder = DiffGraph.newBuilder
    withErrorReporting(part) {
      val variables: Traversal[nodes.Identifier] =
        (part.astMinusRoot.isCall.name("ASSIGN")
          ++ part.astMinusRoot.isCall.name("=")).map(node =>
          node.astChildren.order(0).head match {
            case node: nodes.Identifier => node
            case x =>
              throw ReportableError(
                part,
                part,
                s"unexpected assign element for $part and element $x")
        })
      val globalDecl: List[String] = part.astMinusRoot.isCall
        .name("BIND_GLOBAL")
        .map { node =>
          node.astChildren.order(0).l match {
            case Nil =>
              throw ReportableError(part,
                                    part,
                                    "DECLARE_CONST does not have first child")
            case single :: Nil =>
              single match {
                case string: nodes.Identifier => string.name
              }
            case _ :: _ =>
              throw ReportableError(part,
                                    part,
                                    "DECLARE_CONST has multiple first children")
          }
        }
        .toList
      if (part.astChildren.nonEmpty) {
        var count = part.astChildren.map(_.order).toList.max
        variables
          .filter { variable =>
            !globalDecl.contains(variable.name)
          }
          .foreach { variable =>
            count = count + 1
            val local =
              nodes
                .NewLocal()
                .code(variable.name)
                .name(variable.name)
                .order(count)
            diffGraph.addNode(local)
            diffGraph.addEdge(part, local, EdgeTypes.AST)
          }
      }
    }
    Iterator(diffGraph.build())
  }
}

object LocalIdentificationPass {

  def getRelevantMethodDeclarations(cpg: Cpg): Seq[nodes.Method] = {
    cpg.method.l
  }

}
