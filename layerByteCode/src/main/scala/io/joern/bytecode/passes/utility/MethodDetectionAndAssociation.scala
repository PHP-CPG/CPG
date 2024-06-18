package io.joern.bytecode.passes.utility

import io.joern.bytecode.Defines
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method}
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.Traversal

import java.io.{ByteArrayOutputStream, PrintWriter}
import scala.sys.process.{ProcessLogger, _}

object MethodDetectionAndAssociation {

  val KNOWN_FUNCTION_STARTS: Set[String] = Set("INIT_FCALL",
                                               "INIT_METHOD_CALL",
                                               "INIT_DYNAMIC_CALL",
                                               "INIT_FCALL_BY_NAME",
                                               "INIT_NS_FCALL_BY_NAME",
                                               "INIT_STATIC_METHOD_CALL",
                                               "NEW",
                                               "INIT_USER_CALL")

  val KNOWN_FUNCTION_ENDS: Set[String] = Set(
    "DO_FCALL_BY_NAME",
    "DO_ICALL",
    "DO_UCALL",
    "DO_FCALL"
  )
  val KNOWN_SEND_VALS: Set[String] = Set(
    "SEND_VAR_NO_REF_EX",
    "SEND_VAR_NO_REF",
    "SEND_VAR_EX",
    "SEND_VAL_EX",
    "SEND_FUNC_ARG",
    "SEND_VAL",
    "SEND_VAR",
    "SEND_REF",
    "SEND_ARRAY",
    "SEND_USER"
  )

  var phpInternalFunctions: Option[Set[String]] = None

  def getPhpInternalFunctions(interpreter: String): Set[String] = {
    phpInternalFunctions match {
      case Some(x) => x
      case None =>
        val stderrStream = new ByteArrayOutputStream()
        val stdoutStream = new ByteArrayOutputStream()
        val stdoutWriter = new PrintWriter(stdoutStream)
        val stderrWriter = new PrintWriter(stderrStream)
        val command =
          s"""$interpreter -r "echo implode(',',get_defined_functions()['internal']);""""
        command.!(ProcessLogger(stdoutWriter.println, stderrWriter.println))
        stderrWriter.close()
        stdoutWriter.close()
        phpInternalFunctions = Some(stdoutStream.toString.split(",").toSet)
        phpInternalFunctions.get
    }
  }

  def getSendOrderLiteral(node: nodes.Call): Int = {
    val pos = node.name match {
      case "SEND_ARRAY"                     => 0
      case x if KNOWN_SEND_VALS.contains(x) => 1
    }
    node.astChildren.order(pos).head.asInstanceOf[nodes.Literal].code.toInt
  }

  def getSendValuePos(node: nodes.Call): Int = {
    val pos = node.name match {
      case "SEND_ARRAY"                     => 1
      case x if KNOWN_SEND_VALS.contains(x) => 0
    }
    pos
  }

  private def resolveThisAndSelf(call: Call): Option[String] = {
    val split = AstTraversal
      .getParentMethod(call)
      .fullName
      .split("::")
    if (split.length == 2) {
      Some(split.head)
    } else {
      None
    }
  }

  def handleDynamicCall(call: Call): String = {
    "DYNAMIC"
  }

  def handleUserCall(call: Call): String = {
    "DYNAMIC"
  }

  def handleCallByName(call: Call): String = {
    call.astChildren.order(1).head.asInstanceOf[nodes.Literal].code
  }

  def handleStaticMethodCall(call: Call): String = {
    val callSize = call.astChildren.l.length
    val tclass = call.astChildren.order(1).next() match {
      case x: nodes.Literal if x.code == "self" => resolveThisAndSelf(call)
      case x: nodes.Literal                     => Some(x.code)
      case _                                    => None
    }
    val target =
      call.astChildren.order(if (callSize == 3) 2 else 3).next() match {
        case x: nodes.Literal => Some(x.code)
        case _                => None
      }
    (tclass, target) match {
      case (Some(className), Some(methodName)) =>
        s"$className::$methodName"
      case (_, Some(methodName)) => s".*::$methodName"
      case _                     => Defines.DYNAMIC_FUNCTION
    }
  }

  def handleFcall(call: Call): String = {
    call.astChildren.order(2).head.asInstanceOf[nodes.Literal].code
  }

  def handleMethodCall(call: Call): String = {
    (call.astChildren.order(1).head, call.astChildren.order(2).head) match {
      case (target: nodes.Identifier, node: nodes.Literal) =>
        target.code match {
          case "THIS" =>
            val parentClass = resolveThisAndSelf(call)
            parentClass match {
              case Some(value) =>
                if (value.contains("{closure}")) {
                  s".*::${node.code}"
                } else {
                  s"$value::${node.code}"
                }
              case None => s".*::${node.code}"
            }

          case _ => s".*::${node.code}"
        }
      case (_, _) => Defines.DYNAMIC_FUNCTION
    }
  }

  def handleNew(call: Call): String = {
    call.astChildren.order(1).head match {
      case name: nodes.Literal => s"${name.code}::__construct"
      case _: nodes.Identifier => s"UNKNOWN::__construct"
    }
  }

  def getCalledMethod(initCall: Call): String = {
    val ret: String = initCall.name match {
      case "INIT_DYNAMIC_CALL" => handleDynamicCall(initCall)
      case "INIT_USER_CALL"    => handleUserCall(initCall)
      case "INIT_FCALL_BY_NAME" | "INIT_NS_FCALL_BY_NAME" =>
        handleCallByName(initCall)
      case "INIT_STATIC_METHOD_CALL" => handleStaticMethodCall(initCall)
      case "INIT_FCALL"              => handleFcall(initCall)
      case "INIT_METHOD_CALL"        => handleMethodCall(initCall)
      case "NEW"                     => handleNew(initCall)
    }
    ret.toLowerCase
  }

  def getCallCorrespondingMethod(cpg: Cpg, name: String): List[Method] = {
    name.toLowerCase.replace("\\", "\\\\") match {
      case "DYNAMIC"                  => Nil
      case calledFunctionName: String =>
        // we actually want the regexp feature here!
        val traversal: Traversal[Method] =
          cpg.method.fullName(calledFunctionName)
        traversal.l
    }
  }

}
