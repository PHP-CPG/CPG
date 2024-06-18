package io.joern.bytecode.passes.utility

import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.codepropertygraph.generated.nodes.AstNode
import io.shiftleft.semanticcpg.language._

import scala.annotation.tailrec

object AstTraversal {

  @tailrec
  def getParentMethod(node: AstNode): nodes.Method = {
    node match {
      case x: nodes.Method =>
        x
      case x => getParentMethod(x.astParent.next())
    }
  }

}
