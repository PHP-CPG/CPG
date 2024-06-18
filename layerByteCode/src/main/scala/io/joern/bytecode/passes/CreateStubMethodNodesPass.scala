package io.joern.bytecode.passes

import io.joern.bytecode.Defines
import io.joern.bytecode.passes.utility.MethodDetectionAndAssociation._
import io.joern.reporting.{Linking, Reporting}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{
  Call,
  Method,
  NewMethod,
  NewMethodReturn
}
import io.shiftleft.passes.{CpgPass, DiffGraph, IntervalKeyPool}
import io.shiftleft.semanticcpg.language._

import scala.collection.mutable.{Map => MMap}

class CreateStubMethodNodesPass(cpg: Cpg,
                                keyPool: IntervalKeyPool,
                                strict: Boolean,
                                interpreter: String)
    extends CpgPass(cpg, keyPool = Some(keyPool))
    with Reporting
    with Linking {

  override val name: String = "CreateStubMethodNodesPass"

  private val handledDanglingMethods: MMap[String, NewMethod] = MMap()

  val userlandMethods: List[Method] = cpg.method.l

  private def getAllMethodInitializations: List[Call] = {
    KNOWN_FUNCTION_STARTS.toList.flatMap { name =>
      cpg.call.nameExact(name).l
    }
  }

  private def callStartsWithWildcard(c: Call) =
    getCalledMethod(c).startsWith(".*")

  override def run(): Iterator[DiffGraph] = {
    val diffGraph: DiffGraph.Builder = DiffGraph.newBuilder
    withErrorReporting() {
      /*
      Order the calls in such a way that all non-wildcard calls come first.
      This should prevent the following error:
      - Wildcard `.*::foo` comes first, matches one existing thing
      - Call `qualified::foo` comes second, gets new stub

      Finish-up pass:
      - Wildcard is processed, matches both the existing thing and the qualified thing -> error
       */
      val sorted = getAllMethodInitializations.sortWith {
        case (a, b)
            if !callStartsWithWildcard(a) && !callStartsWithWildcard(b) =>
          a.id <= b.id
        case (a, b)
            if !callStartsWithWildcard(a) && callStartsWithWildcard(b) =>
          true
        case (a, b)
            if callStartsWithWildcard(a) && !callStartsWithWildcard(b) =>
          false
        case (a, b) if callStartsWithWildcard(a) && callStartsWithWildcard(b) =>
          a.id <= b.id
      }

      sorted.foreach { functionStart =>
        processFunctionStart(functionStart, diffGraph)
      }
    }
    Iterator(diffGraph.build())
  }

  private def createStubMethodNode(name: String,
                                   graph: DiffGraph.Builder,
                                   multiple: Boolean): NewMethod = {
    val noNamespaceName = name.split("\\\\").last
    if (handledDanglingMethods.contains(name) || //if we already have a corresponding dangling method
        // or a corresponding internal dangling method
        (handledDanglingMethods.contains(noNamespaceName) && handledDanglingMethods(
          noNamespaceName).code == Defines.INTERNAL_FUNCTION)) {
      // return that stub
      handledDanglingMethods.getOrElse(name,
                                       handledDanglingMethods(noNamespaceName))
    } else if (getPhpInternalFunctions(interpreter).contains(noNamespaceName)) { // if the name without namespace is PHP internal
      if (userlandMethods.map(_.name).contains(noNamespaceName)) {
        // a user defined method exists, that shadows a built-in
        // this occurs for example with polyfills
        unableToLinkMethod(name)
        val ret = NewMethod()
          .name(name)
          .fullName(name)
          .code(Defines.MULTIPLE_TARGETS)
          .isExternal(true)
        handledDanglingMethods.addOne(name -> ret)
        graph.addNode(ret)
        ret
      } else {
        unlinkedInternalFunction(noNamespaceName)
        val ret = NewMethod()
          .name(noNamespaceName)
          .fullName(noNamespaceName)
          .code(Defines.INTERNAL_FUNCTION)
          .isExternal(true)
        handledDanglingMethods.addOne(noNamespaceName -> ret)
        graph.addNode(ret)
        ret
      }
    }
    // if the method is dynamic
    else if (name == Defines.DYNAMIC_FUNCTION) {
      // we cannot do anything here
      unableToLinkFunction(name)
      val ret =
        NewMethod()
          .name(name)
          .fullName(name)
          .code(Defines.DYNAMIC_FUNCTION)
          .isExternal(true)
      handledDanglingMethods.addOne(name -> ret)
      graph.addNode(ret)
      ret
    }
    // if the method name indicates a constructor
    else if (name.contains("__construct")) {
      unableToLinkMethod(name)
      val ret = NewMethod()
        .name(name)
        .fullName(name)
        .code(if (multiple) Defines.MULTIPLE_TARGETS else Defines.CONSTRUCTOR)
        .isExternal(true)
      handledDanglingMethods.addOne(name -> ret)
      graph.addNode(ret)
      ret
    }
    // if the method name indicates another method
    else if (name.contains("::")) {
      unableToLinkMethod(name)
      val ret = NewMethod()
        .name(name)
        .fullName(name)
        .code(
          if (multiple) Defines.MULTIPLE_TARGETS else Defines.UNKNOWN_METHOD)
        .isExternal(true)
      handledDanglingMethods.addOne(name -> ret)
      graph.addNode(ret)
      ret
    }
    // if none of the above we clearly have a unknown function
    else {
      unableToLinkFunction(name)
      val ret = NewMethod()
        .name(name)
        .fullName(name)
        .code(
          if (multiple) Defines.MULTIPLE_TARGETS else Defines.UNKNOWN_FUNCTION)
        .isExternal(true)
      handledDanglingMethods.addOne(name -> ret)
      graph.addNode(ret)
      ret
    }
  }

  private def createStubMethodReturn(stub: NewMethod,
                                     graph: DiffGraph.Builder) = {
    val method_return = NewMethodReturn()
      .lineNumber(stub.lineNumber)
      .columnNumber(stub.columnNumber)
    graph.addNode(method_return)
    graph.addEdge(stub, method_return, EdgeTypes.AST)
    graph.addEdge(stub, method_return, EdgeTypes.CFG)
  }

  private def processFunctionStart(functionStart: Call,
                                   graph: DiffGraph.Builder): Unit = {

    val name = getCalledMethod(functionStart)
    val unknownMethodName = name.replace(".*", "UNKNOWN")
    getCallCorrespondingMethod(cpg, name) match {
      // if there is no method we do need to add a dangling method and note that we cannot link
      case Nil =>
        val stub =
          createStubMethodNode(unknownMethodName, graph, multiple = false)
        createStubMethodReturn(stub, graph)
      // if there is a single corresponding method
      case _ :: Nil
          if !handledDanglingMethods.exists(
            x => // however there must not be already a stub
              if (name.startsWith(".*")) { // for a corresponding name via wildcard
                name.r.matches(x._1)
              } else {
                x._1 == name // or exact match
            }) =>
        // if there is a unique called method IN THE CPG we do not need to add a dangling method node as we know we can link
        if (name.contains("::")) {
          linkedMethod(name)
        } else {
          linkedFunction(name)
        }
      // if there are multiple methods it depends on the config - if strict we need to add a dangling method
      case _ if strict =>
        val stub =
          createStubMethodNode(unknownMethodName, graph, multiple = true)
        createStubMethodReturn(stub, graph)
      // if not strict no action is required
      case _ =>
    }
  }

}
