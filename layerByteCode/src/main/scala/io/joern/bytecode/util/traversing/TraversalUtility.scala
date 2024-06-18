package io.joern.bytecode.util.traversing

import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.{Traversal, jIteratortoTraversal}

import scala.annotation.tailrec
import scala.jdk.CollectionConverters._

object TraversalUtility {

  @tailrec
  def getParentMethod(node: AstNode): Method = {
    node match {
      case x: Method => x
      case x =>
        getParentMethod(x.astParent)
    }
  }

  def getNthArgument(call: Call, nth: Int): CfgNode = {
    call
      .out(EdgeTypes.ARGUMENT)
      .asScala
      .find {
        case node: CfgNode =>
          node.astParent match {
            case node: Call =>
              node.name match {
                case "SEND_VAR_NO_REF_EX" | "SEND_VAR_NO_REF" | "SEND_VAR_EX" |
                    "SEND_VAL_EX" | "SEND_FUNC_ARG" | "SEND_VAL" | "SEND_VAR" |
                    "SEND_REF" | "SEND_USER" =>
                  node.astChildren
                    .order(1)
                    .next()
                    .asInstanceOf[Literal]
                    .code
                    .toInt == nth
                case "SEND_ARRAY" =>
                  node.astChildren
                    .order(0)
                    .next()
                    .asInstanceOf[Literal]
                    .code
                    .toInt == nth
                case _ => false
              }
            case _ => false
          }
      }
      .getOrElse(throw new RuntimeException(
        s"call ${call.code} as no $nth argument"))
      .asInstanceOf[CfgNode]
  }

  def followReachingForward(call: Call,
                            variable: String): Traversal[CfgNode] = {
    call
      .outE(EdgeTypes.REACHING_DEF)
      .asScala
      .filter { edge =>
        edge.property("VARIABLE").asInstanceOf[String] == variable
      }
      .map(_.inNode().asInstanceOf[CfgNode])
  }

  def followReachingBackwards(call: Call,
                              variable: String): Traversal[CfgNode] = {
    call
      .inE(EdgeTypes.REACHING_DEF)
      .asScala
      .filter { edge =>
        edge.property("VARIABLE").asInstanceOf[String] == variable
      }
      .map(_.outNode().asInstanceOf[CfgNode])
  }

  def followControlFlowForward(node: CfgNode): Traversal[CfgNode] = {
    node.out(EdgeTypes.CFG).asScala.map(_.asInstanceOf[CfgNode])
  }

  def followControlFlowBackwards(node: CfgNode): Traversal[CfgNode] = {
    node.in(EdgeTypes.CFG).asScala.map(_.asInstanceOf[CfgNode])
  }

  def followCallForward(call: CfgNode): Traversal[Method] = {
    call.out(EdgeTypes.CALL).asScala.map(_.asInstanceOf[Method])
  }

  def followCallBackwards(method: Method): Traversal[CfgNode] = {
    method.in(EdgeTypes.CALL).asScala.map(_.asInstanceOf[CfgNode])
  }

  def followBackParameter(method: Method,
                          parameter: Int): Traversal[CfgNode] = {
    followCallBackwards(method).map { callSite =>
      getNthArgument(callSite.asInstanceOf[Call], parameter)
    }
  }

  def ifIsArgGetCaller(call: CfgNode): Option[List[CfgNode]] = {
    if (call.inE(EdgeTypes.ARGUMENT).nonEmpty) {
      return Some(call.in(EdgeTypes.ARGUMENT).map(_.asInstanceOf[CfgNode]).l)
    }
    None
  }
}
