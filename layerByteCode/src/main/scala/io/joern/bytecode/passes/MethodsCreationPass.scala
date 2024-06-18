package io.joern.bytecode.passes

import io.joern.bytecode.Defines
import io.joern.bytecode.parser.constructs._
import io.joern.bytecode.passes.utility.MethodIdentification
import io.joern.bytecode.passes.utility.OperationCreation.addOperation
import io.joern.reporting.{ReportableError, Reporting}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.{
  DispatchTypes,
  EdgeTypes,
  nodes
}
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool, ParallelCpgPass}

import java.io.File

class MethodsCreationPass(
    filesMethodDefinitionPairs: List[Seq[MethodDefinitionPair]],
    cpg: Cpg,
    keyPool: IntervalKeyPool)
    extends ParallelCpgPass[Seq[MethodDefinitionPair]](
      cpg,
      keyPools = Some(keyPool.split(filesMethodDefinitionPairs.size)))
    with Reporting {

  override val name = "MethodsCreationPass"
  override def partIterator: Iterator[Seq[MethodDefinitionPair]] =
    filesMethodDefinitionPairs.iterator

  override def runOnPart(fileMethodDefinitionPairs: Seq[MethodDefinitionPair])
    : Iterator[DiffGraph] = {
    implicit val diffGraph: DiffGraph.Builder = DiffGraph.newBuilder
    try {
      // creating and adding the file node
      val fileNode = createFileNode(
        MethodIdentification.getAbsolutePath(
          fileMethodDefinitionPairs.head.byteCodeBlock.fileName))
      diffGraph.addNode(fileNode)
      // creating and adding the namespace nodes
      createFileNamespaceBlocks(fileNode, fileMethodDefinitionPairs)
      // creating mapping for type declarations
      implicit val typeDeclarations
        : collection.mutable.Map[String, nodes.NewTypeDecl] =
        collection.mutable.Map()
      var counter = 0
      for (MethodDefinitionPair(definitionBlock, _) <- fileMethodDefinitionPairs) {
        createMethod(definitionBlock, counter, fileNode)
        counter = counter + 1
      }
    } catch {
      case x: ReportableError =>
        reportError(x)
      case x: Throwable =>
        if (fileMethodDefinitionPairs.nonEmpty) {
          reportError(fileMethodDefinitionPairs.head.byteCodeBlock.fileName,
                      "",
                      "",
                      "",
                      x.getMessage,
                      x)
        } else {
          reportWarning("",
                        "",
                        "",
                        "",
                        "we got an empty set of method definitions")
        }
    }
    Iterator(diffGraph.build())
  }

  def createFileNode(path: String): nodes.NewFile = {
    val absolutePath = new File(path).toPath.toAbsolutePath
      .normalize()
      .toString
    nodes.NewFile().name(absolutePath).order(0)
  }

  def createFileNamespaceBlocks(
      fileNode: nodes.NewFile,
      methodDefinitionPairs: Seq[MethodDefinitionPair])(
      implicit graph: DiffGraph.Builder): Unit = {
    var createdNamespaces = Set[String]()
    methodDefinitionPairs.foreach { definitionPair =>
      val namespaceName = definitionPair.byteCodeBlock.namespace match {
        case Some(namespaceName) => namespaceName
        case None                => Defines.GLOBAL_NAMESPACE_NAME
      }
      if (!createdNamespaces.contains(namespaceName)) {
        createdNamespaces += namespaceName
        val namespaceBlock = nodes
          .NewNamespaceBlock()
          .name(namespaceName)
          .fullName(s"${fileNode.name}:$namespaceName")
          .filename(fileNode.name)
        graph.addNode(namespaceBlock)
        graph.addEdge(fileNode, namespaceBlock, EdgeTypes.AST)
      }
    }
  }

  def getTypeDeclaration(definition: ByteCodeDefinitionsBlock)(
      implicit diffGraph: DiffGraph.Builder,
      typeDeclMap: collection.mutable.Map[String, nodes.NewTypeDecl])
    : nodes.NewTypeDecl = {
    assert(definition.classname.nonEmpty)
    val fullName = definition.namespace match {
      case Some(namespace) => s"$namespace\\${definition.classname.get}"
      case None            => s"${definition.classname.get}"
    }
    if (!typeDeclMap.contains(fullName)) {
      val typeDecl = nodes
        .NewTypeDecl()
        .name(definition.classname.get)
        .fullName(fullName)
        .astParentType("NAMESPACE_BLOCK")
        .astParentFullName(definition.namespace match {
          case Some(namespace) => s"${definition.fileName}:$namespace"
          case None =>
            s"${definition.fileName}:${Defines.GLOBAL_NAMESPACE_NAME}"
        })
      diffGraph.addNode(typeDecl)
      typeDeclMap.addOne((fullName, typeDecl))
    }
    typeDeclMap(fullName)
  }

  def createMethod(methodDefinition: ByteCodeDefinitionsBlock,
                   order: Integer,
                   file: nodes.NewFile)(
      implicit graph: DiffGraph.Builder,
      typeDecl: collection.mutable.Map[String, nodes.NewTypeDecl]): Unit = {
    val code = methodDefinition.name +
      Range
        .inclusive(1, methodDefinition.args)
        .map(c => "$" + s"param$c")
        .mkString("(", ", ", ")")
    // creating and adding method as well as possible type decl node and connection
    val fileName: String =
      MethodIdentification.getAbsolutePath(methodDefinition.fileName)
    val start: Integer = methodDefinition.lineStart
    val end: Integer = methodDefinition.lineEnd
    /*methodDefinition.instructions.head.fileLine match {
      case Some(value) => value
      case None =>
        throw ReportableError(
          methodDefinition.fileName,
          methodDefinition.lineStart,
          methodDefinition.name,
          methodDefinition.instructions.head.instruction.toString,
          "there should be a file line"
        )
    }
    val end: Integer = methodDefinition.instructions.last.fileLine match {
      case Some(value) => value
      case None =>
        throw ReportableError(
          methodDefinition.fileName,
          methodDefinition.lineStart,
          methodDefinition.name,
          methodDefinition.instructions.last.instruction.toString,
          "there should be a file line"
        )
    }*/
    val method = methodDefinition.classname match {
      case Some(_) =>
        val parentType = getTypeDeclaration(methodDefinition)
        val method = nodes
          .NewMethod()
          .name(methodDefinition.name)
          .code(code)
          .fullName(methodDefinition.fullyQualifiedName)
          .order(order)
          .astParentType("TYPE_DECL")
          .astParentFullName(parentType.fullName)
          .filename(fileName)
          .lineNumber(start)
          .lineNumberEnd(end)
        graph.addNode(method)
        graph.addEdge(parentType, method, EdgeTypes.AST)
        method
      case None =>
        methodDefinition.namespace match {
          case Some(namespace) =>
            val method = nodes
              .NewMethod()
              .name(methodDefinition.name)
              .code(code)
              .fullName(methodDefinition.fullyQualifiedName)
              .order(order)
              .astParentType("NAMESPACE_BLOCK")
              .astParentFullName(s"${file.name}:$namespace")
              .filename(fileName)
              .lineNumber(start)
              .lineNumberEnd(end)
            graph.addNode(method)
            method
          case None =>
            val method = nodes
              .NewMethod()
              .name(methodDefinition.name)
              .code(code)
              .fullName(methodDefinition.fullyQualifiedName)
              .order(order)
              .astParentType("NAMESPACE_BLOCK")
              .astParentFullName(
                s"${file.name}:${Defines.GLOBAL_NAMESPACE_NAME}")
              .filename(fileName)
              .lineNumber(start)
              .lineNumberEnd(end)
            graph.addNode(method)
            method
        }
    }
    // creating parameter nodes
    var orderCounter = 0
    for (i <- Range.inclusive(1, methodDefinition.args)) {
      val parameterNode =
        nodes.NewMethodParameterIn().name(s"$i").order(orderCounter)
      graph.addNode(parameterNode)
      graph.addEdge(method, parameterNode, EdgeTypes.AST)
      orderCounter = orderCounter + 1
    }
    // creating the method block node
    val blockNode = nodes.NewBlock().code("METHOD BLOCK").order(orderCounter)
    orderCounter = orderCounter + 1
    graph.addNode(blockNode)
    graph.addEdge(method, blockNode, EdgeTypes.AST)
    // adding the return node
    val returnNode = nodes.NewMethodReturn().order(orderCounter)
    graph.addNode(returnNode)
    graph.addEdge(method, returnNode, EdgeTypes.AST)
    // adding the instructions
    //var blockOrderCounter = 0
    for (instruction <- methodDefinition.instructions) {
      addMethodInstructionLine(instruction, blockNode, methodDefinition)
      //blockOrderCounter = blockOrderCounter + 1
    }
  }

  def addMethodInstructionLine(
      line: InstructionLine,
      block: nodes.NewBlock,
      byteCodeDefinitionBlock: ByteCodeDefinitionsBlock)(
      implicit graph: DiffGraph.Builder): Unit = {

    line.instruction match {
      case instruction: Assignment =>
        val lhs = nodes
          .NewIdentifier()
          .name(instruction.lhs.name)
          .order(0)
          .code(s"${instruction.lhs.name}")
        val rhs = addOperation(
          instruction.rhs,
          order = 1,
          line.fileLine.getOrElse(-1)) 
        val assignment = nodes
          .NewCall()
          .code(s"${lhs.code} = ${rhs.code}")
          .lineNumber(line.fileLine)
          .dispatchType(DispatchTypes.STATIC_DISPATCH)
          .name("=")
          .order(line.opNumber.get)
        graph.addNode(assignment)
        graph.addNode(lhs)
        graph.addEdge(block, assignment, EdgeTypes.AST)
        graph.addEdge(assignment, lhs, EdgeTypes.AST)
        graph.addEdge(assignment, rhs, EdgeTypes.AST)
        graph.addEdge(assignment, lhs, EdgeTypes.ARGUMENT)
        graph.addEdge(assignment, rhs, EdgeTypes.ARGUMENT)
      case instruction: Operation =>
        val call =
          addOperation(instruction.op,
                       line.opNumber.get,
                       line.fileLine.getOrElse(-1)) 
        graph.addEdge(block, call, EdgeTypes.AST)
      case _ =>
        throw ReportableError(
          byteCodeDefinitionBlock.fileName,
          line.fileLine.getOrElse(-1).toString.toInt,
          byteCodeDefinitionBlock.name,
          line.instruction.toString,
          "the instruction is not yet supported"
        )
    }
  }
}
