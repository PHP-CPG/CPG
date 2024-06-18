package io.joern.bytecode.passes.utility

import io.joern.bytecode.parser.constructs._
import io.joern.bytecode.passes.utility.ValueCreation.createValueNode
import io.joern.reporting.ReportableError
import io.shiftleft.codepropertygraph.generated.nodes.NewCall
import io.shiftleft.codepropertygraph.generated.{
  DispatchTypes,
  EdgeTypes,
  nodes
}
import io.shiftleft.passes.DiffGraph

object OperationCreation {

  def addOperation(opcode: Opcode, order: Integer, lineNumber: Integer)(
      implicit graph: DiffGraph.Builder): nodes.NewCall = {
    opcode match {
      case op: NoValueOperation => addNoValueOperation(op, order, lineNumber)
      case op: SingleValueOperation =>
        addSingleValueOperation(op, order, lineNumber)
      case op: DualValueOperation =>
        addDualValueOperation(op, order, lineNumber)
      case op: TripleValueOperation =>
        addTripleValueOperation(op, order, lineNumber)
      case op: QuadrupleValueOperation =>
        addQuadrupleValueOperation(op, order, lineNumber)
      case op: QuintupleValueOperation =>
        addQuintupleValueOperation(op, order, lineNumber)
      case op: INIT_FCALL => addOperationInitFcall(op, order, lineNumber)
      case op: INIT_FCALL_BY_NAME =>
        addOperationInitFcallByName(op, order, lineNumber)
      case op: INIT_DYNAMIC_CALL =>
        addOperationInitDynamicCall(op, order, lineNumber)
      case op: INIT_METHOD_CALL =>
        addOperationInitMethodCall(op, order, lineNumber)
      case op: INIT_NS_FCALL_BY_NAME =>
        addOperationInitFsFcallByName(op, order, lineNumber)
      case op: INIT_STATIC_METHOD_CALL =>
        addOperationInitStaticMethodCall(op, order, lineNumber)
      case op: INIT_USER_CALL =>
        addOperationInitUserCall(op, order, lineNumber)
      case op: SWITCH =>
        addOperationSwitch(op, order, lineNumber)
      case op: MatchOpcode =>
        addOperationMatch(op, order, lineNumber)
    }
  }

  def addQuintupleValueOperation(
      op: QuintupleValueOperation,
      order: Integer,
      lineNumber: Integer)(implicit graph: DiffGraph.Builder): nodes.NewCall = {
    val first = createValueNode(op.first, Integer.valueOf(0))
    val second = createValueNode(op.second, Integer.valueOf(1))
    val third = createValueNode(op.third, Integer.valueOf(2))
    val fourth = createValueNode(op.fourth, Integer.valueOf(3))
    val fifth = createValueNode(op.fifth, Integer.valueOf(4))
    val code =
      s"${op.code} ${first._2} ${second._2} ${third._2} ${fourth._2} ${fifth._2}"
    val operation = nodes
      .NewCall()
      .name(op.code)
      .code(code)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .order(order)
      .lineNumber(Some(lineNumber))
    graph.addNode(operation)
    graph.addNode(first._1)
    graph.addEdge(operation, first._1, EdgeTypes.ARGUMENT)
    graph.addEdge(operation, first._1, EdgeTypes.AST)
    graph.addNode(second._1)
    graph.addEdge(operation, second._1, EdgeTypes.ARGUMENT)
    graph.addEdge(operation, second._1, EdgeTypes.AST)
    graph.addNode(third._1)
    graph.addEdge(operation, third._1, EdgeTypes.ARGUMENT)
    graph.addEdge(operation, third._1, EdgeTypes.AST)
    graph.addNode(fourth._1)
    graph.addEdge(operation, fourth._1, EdgeTypes.ARGUMENT)
    graph.addEdge(operation, fourth._1, EdgeTypes.AST)
    graph.addNode(fifth._1)
    graph.addEdge(operation, fifth._1, EdgeTypes.ARGUMENT)
    graph.addEdge(operation, fifth._1, EdgeTypes.AST)
    operation
  }

  def addQuadrupleValueOperation(
      op: QuadrupleValueOperation,
      order: Integer,
      lineNumber: Integer)(implicit graph: DiffGraph.Builder): nodes.NewCall = {
    val first = createValueNode(op.first, Integer.valueOf(0))
    val second = createValueNode(op.second, Integer.valueOf(1))
    val third = createValueNode(op.third, Integer.valueOf(2))
    val fourth = createValueNode(op.fourth, Integer.valueOf(3))
    val code = s"${op.code} ${first._2} ${second._2} ${third._2} ${fourth._2}"
    val operation = nodes
      .NewCall()
      .name(op.code)
      .code(code)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .order(order)
      .lineNumber(Some(lineNumber))
    graph.addNode(operation)
    graph.addNode(first._1)
    graph.addEdge(operation, first._1, EdgeTypes.ARGUMENT)
    graph.addEdge(operation, first._1, EdgeTypes.AST)
    graph.addNode(second._1)
    graph.addEdge(operation, second._1, EdgeTypes.ARGUMENT)
    graph.addEdge(operation, second._1, EdgeTypes.AST)
    graph.addNode(third._1)
    graph.addEdge(operation, third._1, EdgeTypes.ARGUMENT)
    graph.addEdge(operation, third._1, EdgeTypes.AST)
    graph.addNode(fourth._1)
    graph.addEdge(operation, fourth._1, EdgeTypes.ARGUMENT)
    graph.addEdge(operation, fourth._1, EdgeTypes.AST)
    operation
  }

  def addTripleValueOperation(
      op: TripleValueOperation,
      order: Integer,
      lineNumber: Integer)(implicit graph: DiffGraph.Builder): nodes.NewCall = {
    val first = createValueNode(op.first, Integer.valueOf(0))
    val second = createValueNode(op.second, Integer.valueOf(1))
    val third = createValueNode(op.third, Integer.valueOf(2))
    val code = s"${op.code} ${first._2} ${second._2} ${third._2}"
    val operation = nodes
      .NewCall()
      .code(code)
      .name(op.code)
      .order(order)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .lineNumber(Some(lineNumber))
    graph.addNode(operation)
    graph.addNode(first._1)
    graph.addEdge(operation, first._1, EdgeTypes.ARGUMENT)
    graph.addEdge(operation, first._1, EdgeTypes.AST)
    graph.addNode(second._1)
    graph.addEdge(operation, second._1, EdgeTypes.ARGUMENT)
    graph.addEdge(operation, second._1, EdgeTypes.AST)
    graph.addNode(third._1)
    graph.addEdge(operation, third._1, EdgeTypes.ARGUMENT)
    graph.addEdge(operation, third._1, EdgeTypes.AST)
    operation
  }

  def addDualValueOperation(
      op: DualValueOperation,
      order: Integer,
      lineNumber: Integer)(implicit graph: DiffGraph.Builder): nodes.NewCall = {
    val lhs = createValueNode(op.lhs, Integer.valueOf(0))
    val rhs = createValueNode(op.rhs, Integer.valueOf(1))
    val operation = nodes
      .NewCall()
      .code(s"${op.code} ${lhs._2} ${rhs._2}")
      .name(op.code)
      .order(order)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .lineNumber(Some(lineNumber))
    graph.addNode(operation)
    graph.addNode(lhs._1)
    graph.addEdge(operation, lhs._1, EdgeTypes.ARGUMENT)
    graph.addEdge(operation, lhs._1, EdgeTypes.AST)
    graph.addNode(rhs._1)
    graph.addEdge(operation, rhs._1, EdgeTypes.ARGUMENT)
    graph.addEdge(operation, rhs._1, EdgeTypes.AST)
    operation
  }

  def addSingleValueOperation(
      op: SingleValueOperation,
      order: Integer,
      lineNumber: Integer)(implicit graph: DiffGraph.Builder): nodes.NewCall = {
    val lhs = createValueNode(op.value, Integer.valueOf(0))
    val operation = nodes
      .NewCall()
      .name(op.code)
      .code(s"${op.code} ${lhs._2}")
      .order(order)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .lineNumber(Some(lineNumber))
    graph.addNode(operation)

    graph.addNode(lhs._1)
    graph.addEdge(operation, lhs._1, EdgeTypes.ARGUMENT)
    graph.addEdge(operation, lhs._1, EdgeTypes.AST)
    operation
  }

  def addNoValueOperation(op: NoValueOperation,
                          order: Integer,
                          lineNumber: Integer): nodes.NewCall = {
    nodes
      .NewCall()
      .code(op.code)
      .name(op.code)
      .order(order)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .lineNumber(Some(lineNumber))
  }

  def addOperationSwitch(
      switchstring: SWITCH,
      order: Integer,
      lineNumber: Integer)(implicit graph: DiffGraph.Builder): nodes.NewCall = {
    var counter: Int = 0
    val decisionVar = createValueNode(switchstring.value, counter)
    val fcall = nodes
      .NewCall()
      .name(switchstring.code)
      .code(s"${switchstring.code} ${decisionVar._2} ${switchstring.switches
        .map(switch => "\"" + s"${switch._1}" + "\": " + s"${switch._2}")
        .mkString(", ")}")
      .order(order)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .lineNumber(Some(lineNumber))
    graph.addNode(fcall)
    graph.addNode(decisionVar._1)
    graph.addEdge(fcall, decisionVar._1, EdgeTypes.AST)
    graph.addEdge(fcall, decisionVar._1, EdgeTypes.ARGUMENT)
    counter += 1
    switchstring.switches.foreach { switchPair =>
      val condition = createValueNode(StringLiteral(switchPair._1), counter)._1
      graph.addNode(condition)
      graph.addEdge(fcall, condition, EdgeTypes.AST)
      graph.addEdge(fcall, condition, EdgeTypes.ARGUMENT)
      counter += 1
      val jumpSite =
        createValueNode(IntegerLiteral(switchPair._2.toLong), counter)._1
      graph.addNode(jumpSite)
      graph.addEdge(fcall, jumpSite, EdgeTypes.AST)
      graph.addEdge(fcall, jumpSite, EdgeTypes.ARGUMENT)
      counter += 1
    }
    fcall
  }

  def addOperationMatch(
      opcode: MatchOpcode,
      order: Integer,
      lineNumber: Integer)(implicit graph: DiffGraph.Builder): nodes.NewCall = {
    var counter = 0
    val matchVar = createValueNode(opcode.matchee, counter)
    val fcall = nodes
      .NewCall()
      .name(opcode.code)
      .code(s"${opcode.code} ${matchVar._2} ${(opcode.values.map(x =>
        x.key match {
          case Left(int)     => s"$int: ${x.value}"
          case Right(string) => "\"" + string + "\": " + x.value
      }) :+ s"default: ${opcode.default}").mkString(", ")}")
      .order(order)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .lineNumber(Some(lineNumber))
    graph.addNode(fcall)
    graph.addNode(matchVar._1)
    graph.addEdge(fcall, matchVar._1, EdgeTypes.AST)
    graph.addEdge(fcall, matchVar._1, EdgeTypes.ARGUMENT)
    counter += 1
    opcode.values.foreach(pair => {
      val condition = createValueNode(pair.key match {
        case Left(int)     => IntegerLiteral(int.toLong)
        case Right(string) => StringLiteral(string)
      }, counter)._1
      graph.addNode(condition)
      graph.addEdge(fcall, condition, EdgeTypes.AST)
      graph.addEdge(fcall, condition, EdgeTypes.ARGUMENT)
      counter += 1
      val jumpSite =
        createValueNode(IntegerLiteral(pair.value.toLong), counter)._1
      graph.addNode(jumpSite)
      graph.addEdge(fcall, jumpSite, EdgeTypes.AST)
      graph.addEdge(fcall, jumpSite, EdgeTypes.ARGUMENT)
      counter += 1
    })
    // default case
    // fixme limitation?: default vs "default"
    val condition = createValueNode(StringLiteral("default"), counter)._1
    graph.addNode(condition)
    graph.addEdge(fcall, condition, EdgeTypes.AST)
    graph.addEdge(fcall, condition, EdgeTypes.ARGUMENT)
    counter += 1
    val jumpSite =
      createValueNode(IntegerLiteral(opcode.default.toLong), counter)._1
    graph.addNode(jumpSite)
    graph.addEdge(fcall, jumpSite, EdgeTypes.AST)
    graph.addEdge(fcall, jumpSite, EdgeTypes.ARGUMENT)
    counter += 1
    fcall
  }

  def addOperationInitUserCall(
      op: INIT_USER_CALL,
      order: Integer,
      lineNumber: Integer)(implicit graph: DiffGraph.Builder): NewCall = {
    val paramCount = createValueNode(IntegerLiteral(op.param_count.toLong), 0)
    val targetFunc = createValueNode(op.func_type, 1)
    val actOn = createValueNode(op.act_on, 2)
    val fcall = nodes
      .NewCall()
      .code(s"INIT_USER_CALL ${op.param_count} ${targetFunc._2} ${actOn._2}")
      .name("INIT_USER_CALL")
      .order(order)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .lineNumber(Some(lineNumber))
    graph.addNode(fcall)
    graph.addNode(paramCount._1)
    graph.addEdge(fcall, paramCount._1, EdgeTypes.AST)
    graph.addEdge(fcall, paramCount._1, EdgeTypes.ARGUMENT)
    graph.addNode(targetFunc._1)
    graph.addEdge(fcall, targetFunc._1, EdgeTypes.AST)
    graph.addEdge(fcall, targetFunc._1, EdgeTypes.ARGUMENT)
    graph.addNode(actOn._1)
    graph.addEdge(fcall, actOn._1, EdgeTypes.AST)
    graph.addEdge(fcall, actOn._1, EdgeTypes.ARGUMENT)
    fcall
  }

  def addOperationInitDynamicCall(
      op: INIT_DYNAMIC_CALL,
      order: Integer,
      lineNumber: Integer)(implicit graph: DiffGraph.Builder): nodes.NewCall = {
    val paramCount = createValueNode(IntegerLiteral(op.paramCount.toLong), 0)
    val variable = createValueNode(op.variable, 1)
    val fcall = nodes
      .NewCall()
      .name("INIT_DYNAMIC_CALL")
      .code(s"INIT_DYNAMIC_CALL ${op.paramCount} ${variable._2}")
      .order(order)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .lineNumber(Some(lineNumber))
    graph.addNode(fcall)
    graph.addNode(paramCount._1)
    graph.addNode(variable._1)
    graph.addEdge(fcall, paramCount._1, EdgeTypes.ARGUMENT)
    graph.addEdge(fcall, paramCount._1, EdgeTypes.AST)
    graph.addEdge(fcall, variable._1, EdgeTypes.ARGUMENT)
    graph.addEdge(fcall, variable._1, EdgeTypes.AST)
    fcall
  }

  def addOperationInitFcallByName(
      op: INIT_FCALL_BY_NAME,
      order: Integer,
      lineNumber: Integer)(implicit graph: DiffGraph.Builder): nodes.NewCall = {
    val paramCount = createValueNode(IntegerLiteral(op.paramCount.toLong), 0)
    val functionName = createValueNode(StringLiteral(op.function), 1)
    val fcall = nodes
      .NewCall()
      .code(
        s"INIT_FCALL_BY_NAME ${op.paramCount} " + "string(\"" + op.function + "\")")
      .name(s"INIT_FCALL_BY_NAME")
      .order(order)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .lineNumber(Some(lineNumber))
    graph.addNode(fcall)
    graph.addNode(paramCount._1)
    graph.addNode(functionName._1)
    graph.addEdge(fcall, paramCount._1, EdgeTypes.AST)
    graph.addEdge(fcall, paramCount._1, EdgeTypes.ARGUMENT)
    graph.addEdge(fcall, functionName._1, EdgeTypes.AST)
    graph.addEdge(fcall, functionName._1, EdgeTypes.ARGUMENT)
    fcall
  }

  def addOperationInitFcall(
      op: INIT_FCALL,
      order: Integer,
      lineNumber: Integer)(implicit graph: DiffGraph.Builder): nodes.NewCall = {
    val paramCount = createValueNode(IntegerLiteral(op.paramCount.toLong), 0)
    val var2 = createValueNode(IntegerLiteral(op.var2.toLong), 1)
    val function = createValueNode(op.function, 2)
    val init_fcall = nodes
      .NewCall()
      .code(s"INIT_FCALL ${op.paramCount} ${op.var2} ${function._2}")
      .name(s"INIT_FCALL")
      .order(order)
      .lineNumber(Some(lineNumber))
    graph.addNode(init_fcall)
    graph.addNode(paramCount._1)
    graph.addEdge(init_fcall, paramCount._1, EdgeTypes.AST)
    graph.addEdge(init_fcall, paramCount._1, EdgeTypes.ARGUMENT)
    graph.addNode(var2._1)
    graph.addEdge(init_fcall, var2._1, EdgeTypes.AST)
    graph.addEdge(init_fcall, var2._1, EdgeTypes.ARGUMENT)
    graph.addNode(function._1)
    graph.addEdge(init_fcall, function._1, EdgeTypes.AST)
    graph.addEdge(init_fcall, function._1, EdgeTypes.ARGUMENT)
    init_fcall
  }

  def addOperationInitMethodCall(
      op: INIT_METHOD_CALL,
      order: Integer,
      lineNumber: Integer)(implicit graph: DiffGraph.Builder): nodes.NewCall = {
    val paramCount = createValueNode(IntegerLiteral(op.paramCount.toLong), 0)
    val variable = createValueNode(op.objectVar, 1)
    val method = createValueNode(op.method, 2)
    val init_fcall = nodes
      .NewCall()
      .code(s"INIT_METHOD_CALL ${op.paramCount} ${variable._2} ${method._2}")
      .name(s"INIT_METHOD_CALL")
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .order(order)
      .lineNumber(Some(lineNumber))
    graph.addNode(init_fcall)
    graph.addNode(paramCount._1)
    graph.addEdge(init_fcall, paramCount._1, EdgeTypes.AST)
    graph.addEdge(init_fcall, paramCount._1, EdgeTypes.ARGUMENT)
    graph.addNode(variable._1)
    graph.addEdge(init_fcall, variable._1, EdgeTypes.AST)
    graph.addEdge(init_fcall, variable._1, EdgeTypes.ARGUMENT)
    graph.addNode(method._1)
    graph.addEdge(init_fcall, method._1, EdgeTypes.AST)
    graph.addEdge(init_fcall, method._1, EdgeTypes.ARGUMENT)
    init_fcall
  }

  def addOperationInitFsFcallByName(
      op: INIT_NS_FCALL_BY_NAME,
      order: Integer,
      lineNumber: Integer)(implicit graph: DiffGraph.Builder): nodes.NewCall = {
    val paramCount = createValueNode(IntegerLiteral(op.paramCount.toLong), 0)
    val function = createValueNode(StringLiteral(op.function), 1)
    val init_fcall = nodes
      .NewCall()
      .code(s"INIT_NS_FCALL_BY_NAME ${op.paramCount} ${op.function}")
      .name(s"INIT_NS_FCALL_BY_NAME")
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .order(order)
      .lineNumber(Some(lineNumber))
    graph.addNode(init_fcall)
    graph.addNode(paramCount._1)
    graph.addEdge(init_fcall, paramCount._1, EdgeTypes.AST)
    graph.addEdge(init_fcall, paramCount._1, EdgeTypes.ARGUMENT)
    graph.addNode(function._1)
    graph.addEdge(init_fcall, function._1, EdgeTypes.AST)
    graph.addEdge(init_fcall, function._1, EdgeTypes.ARGUMENT)
    init_fcall
  }

  def addOperationInitStaticMethodCall(
      op: INIT_STATIC_METHOD_CALL,
      order: Integer,
      lineNumber: Integer)(implicit graph: DiffGraph.Builder): nodes.NewCall = {
    val paramCount = createValueNode(IntegerLiteral(op.paramCount.toLong), 0)._1
    val methodString = op.method match {
      case StringLiteral(value) => value
      case x                    => x.toString
    }
    val classString = op.baseClass match {
      case Some(name: StringLiteral) => s" ${name.value}"
      case Some(value)               => s" ${value.toString}"
      case None                      => ""
    }
    var positionCounter = 0
    val first: Option[nodes.NewNode] = op.firstKeyWord match {
      case Some(x) =>
        positionCounter += 1
        Some(createValueNode(x, positionCounter)._1)
      case None => None
    }
    val second: Option[nodes.NewNode] = op.secondKeyword match {
      case Some(x) =>
        positionCounter += 1
        Some(createValueNode(x, positionCounter)._1)
      case None => None
    }
    val classNode: Option[nodes.NewNode] = op.baseClass match {
      case Some(x) =>
        positionCounter += 1
        Some(createValueNode(x, positionCounter)._1)
      case None => None
    }
    val method: String = (op.firstKeyWord, op.secondKeyword) match {
      case (Some(first), Some(second)) =>
        s"($first) ($second)$classString::$methodString"
      case (Some(first), None) => s"($first)$classString::$methodString"
      case (None, None)        => s"$classString::${op.method}"
      case x =>
        throw ReportableError("",
                              lineNumber,
                              "",
                              op.toString,
                              s"unexpected match combination $x")
    }
    positionCounter += 1
    val methodNode = createValueNode(op.method, positionCounter)._1
    val init_fcall = nodes
      .NewCall()
      .code(s"INIT_STATIC_METHOD_CALL ${op.paramCount} $method")
      .name(s"INIT_STATIC_METHOD_CALL")
      .order(order)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .lineNumber(Some(lineNumber))
    graph.addNode(init_fcall)
    List(Some(paramCount), first, second, classNode, Some(methodNode)).foreach {
      case Some(x) =>
        graph.addNode(x)
        graph.addEdge(init_fcall, x, EdgeTypes.AST)
        graph.addEdge(init_fcall, x, EdgeTypes.ARGUMENT)
      case None =>
    }
    init_fcall
  }
}
