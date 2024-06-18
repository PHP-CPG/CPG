package io.joern.bytecode.util.extensions

import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

object NodeExtension {

  implicit class ExtendedAST(node: AstNode) {

    def getParentMethod: Option[Method] = {
      node match {
        case method: Method =>
          Some(method)
        case _ =>
          val parent = this.node.in(EdgeTypes.AST)
          if (parent.hasNext) {
            parent.next().asInstanceOf[AstNode].getParentMethod
          } else {
            None
          }
      }
    }

  }

  implicit class ExtendedCFG(node: CfgNode) {

    def getParentMethod: Option[Method] = {
      node.asInstanceOf[AstNode].getParentMethod
    }

  }

  implicit class ExtendedMethod(method: Method) {

    def getLocation: (String, Int) = {
      val line: Integer = method.lineNumber.getOrElse(-1)
      (method.property("FILENAME").asInstanceOf[String], line.toInt)
    }

  }

  implicit class ExtendedCall(call: Call) {

    class NotAMethodOrFunctionCall(call: Call) extends Throwable {
      override def toString: String =
        s"${call.code} is not a method or function call"
    }

    private def parameterMap(
        call: Call,
        applyToPositional: Literal => Boolean): List[CfgNode] = {
      call.argumentOut
        .collectAll[Call]
        .map { sendVal =>
          sendVal.name match {
            case "SEND_ARRAY" =>
              if (applyToPositional(
                    sendVal.astChildren
                      .order(0)
                      .l
                      .head
                      .asInstanceOf[Literal])) {
                Some(sendVal.astChildren.order(1).l.head.asInstanceOf[CfgNode])
              } else {
                None
              }
            case _ =>
              if (applyToPositional(
                    sendVal.astChildren
                      .order(1)
                      .l
                      .head
                      .asInstanceOf[Literal])) {
                Some(sendVal.astChildren.order(0).l.head.asInstanceOf[CfgNode])
              } else {
                None
              }
          }
        }
        .filter(_.nonEmpty)
        .map(_.get)
        .l
    }

    /**
      * Retrieve a parameter of a call by its position. The index is taken directly from the bytecode, i.e.:
      1-indexed, i.e. f(1,2); but if send_array is used it is in pos 0.
      * @param pos
      * @return
      */
    def getParameter(pos: Int): Option[CfgNode] = {
      def castAndCompare(lit: Literal): Boolean = {
        try {
          lit.code.toInt == pos
        } catch {
          case _: Throwable => false
        }
      }
      parameterMap(call, castAndCompare) match {
        case Nil           => None
        case single :: Nil => Some(single)
        case multiple =>
          throw new RuntimeException(
            s"multiple arguments matching the description ${multiple.map(_.code)}")
      }
    }

    def getParameter(name: String): Option[CfgNode] = {
      parameterMap(call, lit => lit.code == name) match {
        case Nil           => None
        case single :: Nil => Some(single)
        case multiple =>
          throw new RuntimeException(
            s"multiple arguments matching the description ${multiple.map(_.code)}")
      }
    }

    def getParameter(pos: Int, name: String): Option[CfgNode] = {
      // name superseeds position
      getParameter(name) match {
        case Some(value) => Some(value)
        case None        => getParameter(pos)
      }
    }

  }

}
