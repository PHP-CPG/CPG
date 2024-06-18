package io.joern.bytecode.passes

import io.joern.bytecode.util.extensions.NodeExtension.ExtendedCFG
import io.joern.reporting.{ReportableError, Reporting}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, nodes}
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool, ParallelCpgPass}
import io.shiftleft.semanticcpg.language._

class InheritancePass(methods: Seq[nodes.Call],
                      cpg: Cpg,
                      keyPool: IntervalKeyPool)
    extends ParallelCpgPass[nodes.Call](cpg,
                                        keyPools =
                                          Some(keyPool.split(methods.size)))
    with Reporting {

  override val name = "InheritancePass"
  override def partIterator: Iterator[nodes.Call] = methods.iterator

  override def runOnPart(part: nodes.Call): Iterator[DiffGraph] = {
    implicit val diffGraph: DiffGraph.Builder = DiffGraph.newBuilder
    withErrorReporting(part.getParentMethod.get) {
      part.code match {
        case "DECLARE_CLASS_DELAYED" =>
          val List(lhs, rhs) =
            part.astChildren.isLiteral.l.sortWith((lhs, rhs) =>
              lhs.order <= rhs.order) match {
              case x if x.length != 2 =>
                throw ReportableError(
                  part.getParentMethod.get,
                  part,
                  s"only has ${x.length} of the expected 2 children")
              case lhs :: rhs :: Nil =>
                List(lhs, rhs)
            }
          addInheritanceEdge(lhs, rhs, cpg)
        case _ =>
          throw ReportableError(part.getParentMethod.get,
                                part,
                                "given bytecode does not support inheritance")
      }
    }
    Iterator(diffGraph.build())
  }

  def addInheritanceEdge(
      childClass: nodes.Literal,
      parentClass: nodes.Literal,
      cpg: Cpg)(implicit diffGraph: DiffGraph.Builder): Unit = {
    val childType =
      cpg.typeDecl(childClass.code.replace("\\", "\\\\")).l match {
        case Nil =>
          return
        case hit :: Nil => hit
        case _ :: _ =>
          return
      }
    val parentType =
      cpg.typeDecl(parentClass.code.replace("\\", "\\\'")).l match {
        case Nil =>
          return
        case hit :: Nil => hit
        case _ :: _ =>
          return
      }
    diffGraph.addEdge(childType, parentType, EdgeTypes.INHERITS_FROM)
  }
}

object InheritancePass {

  def getInheritanceIndicatingCalls(cpg: Cpg): Seq[nodes.Call] = {
    cpg.call.code("DECLARE_CLASS_DELAYED").l
  }

}
