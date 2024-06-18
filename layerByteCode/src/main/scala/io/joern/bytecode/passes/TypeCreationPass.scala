package io.joern.bytecode.passes

import io.joern.reporting.{AbortPass, Reporting}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, nodes}
import io.shiftleft.passes.{CpgPass, DiffGraph}
import io.shiftleft.semanticcpg.language._

class TypeCreationPass(typeIndicatingNodes: Seq[nodes.StoredNode], cpg: Cpg)
    extends CpgPass(cpg)
    with Reporting {

  override val name = "TypeCreationPass"

  override def run(): Iterator[DiffGraph] = {
    val diffGraph: DiffGraph.Builder = DiffGraph.newBuilder
    try {
      val knownTypes = collection.mutable.ListBuffer[nodes.NewType]()
      typeIndicatingNodes.foreach {
        case part: nodes.TypeDecl =>
          knownTypes.find(typ => typ.fullName == part.fullName) match {
            case Some(x) => x
            case None =>
              val typeNode = nodes
                .NewType()
                .name(part.name)
                .fullName(part.fullName)
              knownTypes.append(typeNode)
              diffGraph.addNode(typeNode)
              diffGraph.addEdge(typeNode, part, EdgeTypes.REF)
          }
        case node: nodes.Literal =>
          node.typeFullName match {
            case "Integer" | "Float" | "String" | "Boolean" =>
              knownTypes.find(typ => typ.fullName == node.typeFullName) match {
                case Some(x) => x
                case None =>
                  val typeNode = nodes
                    .NewType()
                    .name(node.typeFullName)
                    .fullName(node.typeFullName)
                  knownTypes.append(typeNode)
                  diffGraph.addNode(typeNode)
              }
            case _ =>
          }
      }
    } catch {
      case AbortPass =>
      case x: Throwable =>
        reportError("unknown", "", "", "", x.getMessage, x)
    }
    Iterator(diffGraph.build())
  }
}

object TypeCreationPass {

  def getTypeIndicatingNodes(cpg: Cpg): List[nodes.StoredNode] = {
    (cpg.typeDecl ++ cpg.literal).l
  }

}
