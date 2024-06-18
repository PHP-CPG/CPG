package io.joern.bytecode.passes

import io.joern.bytecode.Defines
import io.joern.bytecode.util.extensions.NodeExtension._
import io.joern.reporting.Reporting
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, nodes}
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool, ParallelCpgPass}
import io.shiftleft.semanticcpg.language._

import scala.jdk.CollectionConverters._

class NamespaceMemberCreationPass(methods: Seq[nodes.Method],
                                  cpg: Cpg,
                                  keyPool: IntervalKeyPool)
    extends ParallelCpgPass[nodes.Method](cpg,
                                          keyPools =
                                            Some(keyPool.split(methods.size)))
    with Reporting {

  override val name = "NamespaceMemberCreationPass"
  override def partIterator: Iterator[Method] = methods.iterator

  override def runOnPart(part: nodes.Method): Iterator[DiffGraph] = {
    val (file, _) = part.getLocation
    implicit val diffGraph: DiffGraph.Builder = DiffGraph.newBuilder
    withErrorReporting(part) {
      travelAddAndDelete(part, file)
    }
    Iterator(diffGraph.build())
  }

  def travelAddAndDelete(cfgNode: nodes.CfgNode, file: String)(
      implicit diffGraph: DiffGraph.Builder,
      visited: scala.collection.mutable.Set[nodes.CfgNode] =
        scala.collection.mutable.Set[nodes.CfgNode]()): Unit = {
    if (!visited.contains(cfgNode) && !cfgNode.isInstanceOf[nodes.Return]) {
      visited.add(cfgNode)
      if (cfgNode.code.length >= "DECLARE_CONST".length &&
          cfgNode.code.substring(0, "DECLARE_CONST".length) == "DECLARE_CONST") {
        cfgNode.astChildren.order(0).toList.head match {
          case name: nodes.Literal =>
            assert(name.typeFullName == "String")
            val varNameSplit: Array[String] = name.code.split("\\\\")
            val varName = varNameSplit.last.toLowerCase
            val nameSpaceName =
              varNameSplit.reverse.tail.reverse
                .mkString("\\")
                .toLowerCase match {
                case "" => Defines.GLOBAL_NAMESPACE_NAME
                case x  => x
              }
            cpg.namespaceBlock
              .nameExact(nameSpaceName.replace("\\", "\\\\"))
              .filename(file)
              .toList match {
              case Nil =>
                reportWarning(
                  cfgNode.getParentMethod.get,
                  cfgNode,
                  s"there is no namespace named '$nameSpaceName' requested by ${cfgNode.code} with '{${varNameSplit
                    .mkString(",")}}' in file $file"
                )

              case single :: Nil =>
                val member =
                  nodes.NewMember().code(cfgNode.code).name(varName)
                diffGraph.addNode(member)
                diffGraph.addEdge(single, member, EdgeTypes.AST)
              case _ =>
                reportWarning(
                  cfgNode.getParentMethod.get,
                  cfgNode,
                  s"there are multiple namespaces named '$nameSpaceName' requested by ${cfgNode.code} with '{${varNameSplit
                    .mkString(",")}}' in file $file"
                )
            }
          case _ => throw new RuntimeException()
        }
      }
      cfgNode._cfgOut.asScala.toList.foreach { node =>
        travelAddAndDelete(node.asInstanceOf[nodes.CfgNode], file)
      }
    }
  }

}

object NamespaceMemberCreationPass {

  def getNamespaceMemberRelevantFunctions(cpg: Cpg): Seq[nodes.Method] = {
    cpg.method("dlr_main").l
  }

}
