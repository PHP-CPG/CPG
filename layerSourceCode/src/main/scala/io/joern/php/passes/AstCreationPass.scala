package io.joern.php.passes

import io.joern.Defines
import io.joern.php.passes.utility.ASTJSON._
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, nodes}
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool, ParallelCpgPass}
import parser.php.FileParser

class AstCreationPass(files: Seq[String], cpg: Cpg, keyPool: IntervalKeyPool)
    extends ParallelCpgPass[String](
      cpg,
      keyPools = Some(keyPool.split(files.size))
    ) {

  override def partIterator: Iterator[String] = files.iterator

  override def runOnPart(part: String): Iterator[DiffGraph] = {
    implicit val diffGraph: DiffGraph.Builder = DiffGraph.newBuilder

    val fileNode = nodes.NewFile().name(part)
    diffGraph.addNode(fileNode)
    val nameSpaceBlock = nodes
      .NewNamespaceBlock()
      .name(Defines.globalNameSpaceName)
      .fullName(fileNode.name)
    diffGraph.addNode(nameSpaceBlock)
    diffGraph.addEdge(fileNode, nameSpaceBlock, EdgeTypes.AST)
    try {
      create(FileParser.parse(part)).foreach { node =>
        diffGraph.addEdge(nameSpaceBlock, node, EdgeTypes.AST)
      }
    } catch {
      case e: MatchError =>
        println(
          s"when trying to create ast for file $part exception ${e.toString} \n ${e.getStackTrace
            .mkString("\n")}")
        throw e
      case e: Error =>
        println(
          s"when trying to create ast for file $part exception ${e.toString} \n ${e.getStackTrace
            .mkString("\n")}")
        throw e
    }
    Iterator(diffGraph.build())
  }

  def createCodeBlock(json: List[Map[String, Any]], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val codeBlock = nodes.NewBlock().order(childNum)
    diffGraph.addNode(codeBlock)
    var counter = 0
    val getChildNum = () => {
      val buff = counter
      counter = counter + 1
      buff
    }
    json.map(create(_, getChildNum())).foreach { node =>
      diffGraph.addEdge(codeBlock, node, EdgeTypes.AST)
    }
    codeBlock
  }

  def create(json: List[Map[String, Any]], counterStart: Int = 0)(
      implicit diffGraph: DiffGraph.Builder): Seq[nodes.NewNode] = {
    var counter = counterStart
    val getChildNum = () => {
      val buff = counter
      counter = counter + 1
      buff
    }
    json.map(create(_, getChildNum()))
  }

  def toCode(node: nodes.NewNode): String = {
    node match {
      case node: nodes.NewCall              => node.code
      case node: nodes.NewControlStructure  => node.code
      case node: nodes.NewIdentifier        => node.code
      case node: nodes.NewLiteral           => node.code
      case node: nodes.NewMethodParameterIn => node.code
      case node: nodes.NewReturn            => node.code
      case node: nodes.NewMethod            => node.code
      case _: nodes.NewBlock                => "CODEBLOCK"
      case _: nodes.NewTypeDecl             => "TYPEDECL"
    }
  }

  def toCode(node: Seq[nodes.NewNode]): Seq[String] = {
    node.map(toCode)
  }

  def create(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    json.getOrElse("nodeType", missingKey("nodeType", json)) match {
      case "Stmt_Expression"  => createStmtExpression(json, childNum)
      case "Stmt_Echo"        => createStmtEcho(json, childNum)
      case "Stmt_Else"        => createCodeBlock(getJsonList("stmts", json), childNum)
      case "Stmt_ElseIf"      => createStmtElseIf(json, childNum)
      case "Stmt_Function"    => createStmtFunction(json, childNum)
      case "Stmt_If"          => createStmtIf(json, childNum)
      case "Stmt_Return"      => createStmtReturn(json, childNum)
      case "Stmt_Switch"      => createStmtSwitch(json, childNum)
      case "Stmt_Case"        => createStmtCase(json, childNum)
      case "Stmt_Break"       => createStmtBreak(json, childNum)
      case "Stmt_Class"       => createStmtClass(json, childNum)
      case "Stmt_ClassConst"  => createStmtClassConst(json, childNum)
      case "Stmt_Property"    => createStmtProperty(json, childNum)
      case "Stmt_ClassMethod" => createStmtClassMethod(json, childNum)
      case "Stmt_Namespace"   => createStmtNamespace(json, childNum)
      case "Stmt_Use"         => createStmtUse(json, childNum)
      case "Stmt_Foreach"     => createStmtForeach(json, childNum)
      case "Stmt_Continue"    => createStmtContinue(json, childNum)
      case "Stmt_Throw"       => createStmtThrow(json, childNum)
      case "Stmt_TryCatch"    => createStmtTryCatch(json, childNum)
      case "Stmt_Catch"       => createStmtCatch(json, childNum)
      case "Stmt_Finally"     => createStmtFinally(json, childNum)
      case "Stmt_Unset"       => createStmtUnset(json, childNum)
      case "Expr_FuncCall"    => createExprFuncCall(json, childNum)
      case "Expr_Variable" | "Identifier" | "VarLikeIdentifier" =>
        createExprVariable(json, childNum)
      case "Expr_ConstFetch"    => createExprConstFetch(json, childNum)
      case "Expr_Assign"        => createExprAssign(json, childNum)
      case "Expr_PropertyFetch" => createExprPropertyFetch(json, childNum)
      case "Expr_New"           => createExprNew(json, childNum)
      case "Expr_MethodCall"    => createExprMethodCall(json, childNum)
      case "Expr_StaticCall"    => createExprStaticCall(json, childNum)
      case "Expr_Instanceof"    => createExprInstanceOf(json, childNum)
      case "Expr_ShellExec"     => createExprShellExec(json, childNum)
      case "Expr_Array"         => createExprArray(json, childNum)
      case "Expr_ArrayItem"     => createExprArrayItem(json, childNum)
      case "Expr_ArrayDimFetch" => createExprArrayDimFetch(json, childNum)
      case "Expr_Empty"         => createExprEmpty(json, childNum)
      case "Expr_Isset"         => createExprIsset(json, childNum)
      case "Expr_Ternary"       => createExprTernary(json, childNum)
      case "Expr_Closure"       => createExprClosure(json, childNum)
      case "Expr_List"          => createExprList(json, childNum)
      case "Expr_StaticPropertyFetch" =>
        createExprStaticPropertyFetch(json, childNum)
      case "Expr_ClassConstFetch" => createExprClassConstFetch(json, childNum)
      case "Expr_BooleanNot" | "Expr_UnaryMinus" | "Expr_UnaryPlus" |
          "Expr_BitwiseNot" | "Expr_PreInc" | "Expr_PreDec" | "Expr_PostInc" |
          "Expr_PostDec" | "Expr_ErrorSuppress" =>
        createExprUnaryOp(json, childNum)
      case "Name_FullyQualified" => createNameFullyQualified(json, childNum)
      case "Arg"                 => createArg(json, childNum)
      case "Param"               => createParam(json, childNum)
      case x: String =>
        val substr_binary = "Expr_BinaryOp_".length
        val substr_scalar = "Scalar_".length
        val substr_assignOp = "Expr_AssignOp_".length
        val substr_exprCast = "Expr_Cast_".length
        if (x.length > substr_binary && x.substring(0, substr_binary) == "Expr_BinaryOp_") {
          createExprBinaryOp(json, childNum)
        } else if (x.length > substr_scalar && x.substring(0, substr_scalar) == "Scalar_") {
          createScalar(json, childNum)
        } else if (x.length > substr_assignOp && x.substring(0, substr_assignOp) == "Expr_AssignOp_") {
          createExprAssignOp(json, childNum)
        } else if (x.length > substr_exprCast && x.substring(0, substr_exprCast) == "Expr_Cast_") {
          createExprCast(json, childNum)
        } else {
          println(format(json))
          throw new RuntimeException(s"nodeType $x not yet supported")
        }
    }
  }

  def translateFlag(flag: Double): List[String] = {
    flag match {
      case 0.0  => List()
      case 1.0  => List("public")
      case 2.0  => List("protected")
      case 4.0  => List("private")
      case 9.0  => List("public", "static")
      case 10.0 => List("protected", "static")
      case 12.0 => List("private", "static")
      case 16.0 => List("abstract")
    }
  }

  def nameNodeToNameString(json: Map[String, Any]): String = {
    val name = getJsonAtom[String]("nodeType", json).get
    assert(name == "Name" || name == "Name_FullyQualified",
           s"$name is not a supported name node type")
    getJsonAtom[List[String]]("parts", json).get
      .mkString(if (name == "Name_FullyQualified") "\\" else "", "\\", "")
  }

  def createNameFullyQualified(value: Map[String, Any], integer: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val name = nameNodeToNameString(value)
    val node =
      nodes
        .NewLiteral()
        .code(name)
        .typeFullName("fullyQualifiedName")
        .order(integer)
    diffGraph.addNode(node)
    node
  }

  def createStmtUnset(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val attributes = getJsonObject("attributes", json).get
    val vars = create(getJsonList("vars", json))
    nodes.NewCall()
    val unset = nodes
      .NewCall()
      .name("unset")
      .code(toCode(vars).mkString("unset(", ",", ")"))
      .order(childNum)
      .lineNumber(getLineStart(attributes))
    diffGraph.addNode(unset)
    vars.foreach(variable => diffGraph.addEdge(unset, variable, EdgeTypes.AST))
    unset
  }

  def createExprList(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val attributes = getJsonObject("attributes", json).get
    val items = create(getJsonList("items", json))
    val expr = nodes
      .NewCall()
      .name("list")
      .code(toCode(items).mkString("list(", ",", ")"))
      .order(childNum)
      .lineNumber(getLineStart(attributes))
    diffGraph.addNode(expr)
    items.foreach { node =>
      diffGraph.addEdge(expr, node, EdgeTypes.AST)
    }
    expr
  }

  def createExprTernary(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val attributes = getJsonObject("attributes", json).get
    val condNode = create(getJsonObject("cond", json).get, 0)
    val ifNode = create(getJsonObject("if", json).get, 1)
    val elseNode = create(getJsonObject("else", json).get, 2)
    val ternary = nodes
      .NewControlStructure()
      .code(s"${toCode(condNode)} ? ${toCode(ifNode)} : ${toCode(elseNode)}")
      .parserTypeName("ternary")
      .lineNumber(getLineStart(attributes))
      .order(childNum)
    diffGraph.addNode(ternary)
    diffGraph.addEdge(ternary, condNode, EdgeTypes.AST)
    diffGraph.addEdge(ternary, ifNode, EdgeTypes.AST)
    diffGraph.addEdge(ternary, elseNode, EdgeTypes.AST)
    ternary
  }

  def createExprIsset(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val attributes = getJsonObject("attributes", json).get
    val vars = create(getJsonList("vars", json))
    val isset = nodes
      .NewCall()
      .name("isset")
      .code(toCode(vars).mkString("isset(", ",", ")"))
      .lineNumber(getLineStart(attributes))
      .order(childNum)
    diffGraph.addNode(isset)
    vars.foreach(variable => diffGraph.addEdge(isset, variable, EdgeTypes.AST))
    isset
  }

  def createStmtFinally(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val attributes = getJsonObject("attributes", json).get
    val stmts = getJsonList("stmts", json)
    val finallyNode = nodes
      .NewControlStructure()
      .code("finally")
      .order(childNum)
      .lineNumber(getLineStart(attributes))
    diffGraph.addNode(finallyNode)
    diffGraph.addEdge(finallyNode, createCodeBlock(stmts, 0), EdgeTypes.AST)
    finallyNode
  }

  def createStmtCatch(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val attributes = getJsonObject("attributes", json).get
    val variable = create(getJsonObject("var", json).get, 0)
    //val types = getJsonList("types", json)
    val stmts = getJsonList("stmts", json)
    val catchNode = nodes
      .NewControlStructure()
      .code(s"catch(${toCode(variable)}")
      .parserTypeName("catch")
      .lineNumber(getLineStart(attributes))
      .order(childNum)
    diffGraph.addNode(catchNode)
    diffGraph.addEdge(catchNode, variable, EdgeTypes.AST)
    diffGraph.addEdge(catchNode, createCodeBlock(stmts, 1), EdgeTypes.AST)
    catchNode
  }

  def createStmtTryCatch(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val attributes = getJsonObject("attributes", json).get
    val stmts = getJsonList("stmts", json)
    val catches = getJsonList("catches", json)
    val finallies = getJsonObject("finally", json)
    val tryCatch = nodes
      .NewControlStructure()
      .code("tryCatch")
      .parserTypeName("tryCatch")
      .lineNumber(getLineStart(attributes))
      .order(childNum)
    diffGraph.addNode(tryCatch)
    diffGraph.addEdge(tryCatch, createCodeBlock(stmts, 0), EdgeTypes.AST)
    var child = 1
    for (catchJson <- catches) {
      diffGraph.addEdge(tryCatch, create(catchJson, childNum), EdgeTypes.AST)
      child = child + 1
    }
    finallies match {
      case Some(finallies) =>
        diffGraph.addEdge(tryCatch, create(finallies, child), EdgeTypes.AST)
      case None =>
    }
    tryCatch
  }

  def createStmtThrow(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val attributes = getJsonObject("attributes", json).get
    val expr = create(getJsonObject("expr", json).get, 0)
    val throwNode = nodes
      .NewControlStructure()
      .code(s"throw ${toCode(expr)}")
      .parserTypeName("throw")
      .order(childNum)
      .lineNumber(getLineStart(attributes))
    diffGraph.addNode(throwNode)
    diffGraph.addEdge(throwNode, expr, EdgeTypes.AST)
    throwNode
  }

  def createStmtClassConst(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val flags = getJsonAtom[Double]("flags", json).get
    assert(flags == 0.0)
    val consts = getJsonList("consts", json)
    val codeBlock = nodes
      .NewBlock()
      .order(childNum)
    var childCounter = 0
    diffGraph.addNode(codeBlock)
    for (const <- consts) {
      val constNode = nodes
        .NewIdentifier()
        .name(getJsonObject("name", const).get match {
          case x: Map[String, Any] => getJsonAtom[String]("name", x).get
        })
        .order(childCounter)
      childCounter = childCounter + 1
      diffGraph.addNode(constNode)
      diffGraph.addEdge(codeBlock, constNode, EdgeTypes.AST)
    }
    codeBlock
  }

  def createExprClosure(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val attributes = getJsonObject("attributes", json).get
    //val uses = getJsonList("uses", json)
    val params = create(getJsonList("params", json))
    val stmts = createCodeBlock(getJsonList("stmts", json), params.length)
    val retunType = getJsonAtom[Any]("returnType", json)
    assert(retunType.isEmpty)
    val byRef = getJsonAtom[Boolean]("byRef", json).get
    assert(!byRef)
    val static = getJsonAtom[Boolean]("static", json).get
    assert(!static)
    val clojure = nodes
      .NewMethod()
      .name("anonymous_clojure")
      .fullName("anonymous_clojure")
      .code(toCode(params).mkString("function(", ",", ") with (tbd)"))
      .lineNumber(getLineStart(attributes))
      .lineNumberEnd(getLineEnd(attributes))
      .order(childNum)
    diffGraph.addNode(clojure)
    params.foreach(node => diffGraph.addEdge(clojure, node, EdgeTypes.AST))
    diffGraph.addEdge(clojure, stmts, EdgeTypes.AST)
    clojure
  }

  def createStmtContinue(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val attributes = getJsonObject("attributes", json).get
    val node = nodes
      .NewControlStructure()
      .code("continue")
      .parserTypeName("continue")
      .lineNumber(getLineStart(attributes))
      .order(childNum)
    diffGraph.addNode(node)
    node
  }

  def createExprEmpty(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val attributes = getJsonObject("attributes", json).get
    val expr = create(getJsonObject("expr", json).get, 0)
    val node = nodes
      .NewCall()
      .code(s"empty(${toCode(expr)}")
      .name("empty")
      .order(childNum)
      .lineNumber(getLineStart(attributes))
    diffGraph.addNode(node)
    diffGraph.addEdge(node, expr, EdgeTypes.AST)
    node
  }

  def createStmtForeach(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val attributes = getJsonObject("attributes", json).get
    val byRef = getJsonAtom[Boolean]("byRef", json).get
    assert(!byRef)
    val expr = create(getJsonObject("expr", json).get, 0)
    val (key, childnum): (Option[nodes.NewNode], Int) =
      getJsonObject("keyVar", json) match {
        case Some(node) => (Some(create(node, 1)), 2)
        case None       => (None, 1)
      }
    val value = create(getJsonObject("valueVar", json).get, childnum)
    val stmts = createCodeBlock(getJsonList("stmts", json), childnum + 1)
    val foreach = nodes
      .NewControlStructure()
      .code(s"foreach ${toCode(expr)} as ${key match {
        case Some(node) => s"${toCode(node)} => "; case None =>
      }} ${toCode(value)}")
      .parserTypeName("foreach")
      .lineNumber(getLineStart(attributes))
      .order(childNum)
    diffGraph.addNode(foreach)
    diffGraph.addEdge(foreach, expr, EdgeTypes.AST)
    key match {
      case Some(node) => diffGraph.addEdge(foreach, node, EdgeTypes.AST)
      case None       =>
    }
    diffGraph.addEdge(foreach, value, EdgeTypes.AST)
    diffGraph.addEdge(foreach, stmts, EdgeTypes.AST)
    foreach
  }

  def createExprClassConstFetch(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val attributes = getJsonObject("attributes", json).get
    val classJson = getJsonObject("class", json).get
    val classNode = getJsonAtom[String]("nodeType", classJson).get match {
      case "Name" =>
        nodes
          .NewIdentifier()
          .code(nameNodeToNameString(classJson))
          .typeFullName("classIdentifier")
          .order(0)
      case _ => create(classJson, 0)
    }
    diffGraph.addNode(classNode)
    val nameNode = create(getJsonObject("name", json).get, 1)
    val node = nodes
      .NewCall()
      .code(s"${toCode(classNode)}::${toCode(nameNode)}")
      .name("classConstFetch")
      .order(childNum)
      .lineNumber(getLineStart(attributes))

    diffGraph.addNode(node)
    diffGraph.addEdge(node, classNode, EdgeTypes.AST)
    diffGraph.addEdge(node, nameNode, EdgeTypes.AST)
    node
  }

  def createExprStaticPropertyFetch(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val attributes = getJsonObject("attributes", json).get
    val className = nodes
      .NewLiteral()
      .code(nameNodeToNameString(getJsonObject("class", json).get))
      .typeFullName("String")
      .order(0)
    diffGraph.addNode(className)
    val name = create(getJsonObject("name", json).get, 1)
    diffGraph.addNode(name)
    val staticFetch = nodes
      .NewCall()
      .code(s"${toCode(className)}::${toCode(name)}")
      .name("staticPropertyFetch")
      .lineNumber(getLineStart(attributes))
      .order(childNum)
    diffGraph.addNode(staticFetch)
    diffGraph.addEdge(staticFetch, className, EdgeTypes.AST)
    diffGraph.addEdge(staticFetch, name, EdgeTypes.AST)
    staticFetch
  }

  def createExprArrayDimFetch(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val attributes = getJsonObject("attributes", json).get
    val variable = create(getJsonObject("var", json).get, 0)
    val dim = getJsonObject("dim", json) match {
      case Some(node) => Some(create(node, 1))
      case None       => None
    }
    val fetch = nodes
      .NewCall()
      .code(s"${toCode(variable)}[${dim match {
        case Some(dim) => toCode(dim); case None => ""
      }}]")
      .name("dimFetch")
      .order(childNum)
      .lineNumber(getLineStart(attributes))
    diffGraph.addNode(fetch)
    diffGraph.addEdge(fetch, variable, EdgeTypes.AST)
    dim match {
      case Some(dim) => diffGraph.addEdge(fetch, dim, EdgeTypes.AST)
      case None      =>
    }
    fetch
  }

  def createExprArrayItem(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val unpack = getJsonAtom[Boolean]("unpack", json).get
    assert(!unpack,
           s"array creation with unpack is not supported: ${format(json)}")
    val byRef = getJsonAtom[Boolean]("byRef", json).get
    assert(!byRef, s"array creation byRef is not supported: ${format(json)}")
    val value = getJsonObject("value", json).get
    getJsonObject("key", json) match {
      case Some(key) =>
        val keyNode = create(key, 0)
        diffGraph.addNode(keyNode)
        val valueNode = create(value, 1)
        diffGraph.addNode(valueNode)
        val keyValue = nodes
          .NewControlStructure()
          .code(s"${toCode(keyNode)} => ${toCode(valueNode)}")
          .parserTypeName("keyValuePair")
          .order(childNum)
        diffGraph.addNode(keyValue)
        diffGraph.addEdge(keyValue, keyNode, EdgeTypes.AST)
        diffGraph.addEdge(keyValue, valueNode, EdgeTypes.AST)
        keyValue
      case None =>
        create(value, childNum)
    }

  }

  def createExprArray(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val attributes = getJsonObject("attributes", json).get
    val items: Seq[nodes.NewNode] = create(getJsonList("items", json))
    val array = nodes
      .NewCall()
      .code(toCode(items).mkString("array(", ",", ")"))
      .name("array")
      .lineNumber(getLineStart(attributes))
      .order(childNum)
    diffGraph.addNode(array)
    items.foreach(item => diffGraph.addEdge(array, item, EdgeTypes.AST))
    array
  }

  def createExprCast(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val attributes = getJsonObject("attributes", json).get
    val castTo =
      getJsonAtom[String]("nodeType", json).get.substring("Expr_Cast_".length)
    val expr = getJsonObject("expr", json).get
    val cast = nodes
      .NewCall()
      .code(s"($castTo)")
      .name("cast")
      .order(childNum)
      .lineNumber(getLineStart(attributes))
    diffGraph.addEdge(cast, create(expr, 0), EdgeTypes.AST)
    cast
  }

  def createStmtUse(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    //println(format(json))
    val attributes = getJsonObject("attributes", json).get
    val utype = getJsonAtom[Double]("type", json).get
    val typeNode = nodes
      .NewLiteral()
      .code(utype match {
        case 1.0 => "general"
        case 2.0 => "function"
        case 3.0 => "const"
      })
      .typeFullName("keyword")
    diffGraph.addNode(typeNode)
    val uses = getJsonList("uses", json)
    assert(uses.length == 1)
    val name = getJsonObject("name", uses.head).get match {
      case name: Map[String, Any] =>
        nodes
          .NewLiteral()
          .code(nameNodeToNameString(name))
          .typeFullName("namespaceName")
          .order(1)
    }
    diffGraph.addNode(name)
    val alias: Option[nodes.NewNode] = getJsonObject("alias", uses.head) match {
      case Some(x) => Option(create(x, 2))
      case None    => None
    }
    alias match {
      case Some(x) => diffGraph.addNode(x)
      case None    =>
    }
    val use = nodes
      .NewCall()
      .code("use")
      .name("use")
      .order(childNum)
      .lineNumber(getLineStart(attributes))
    diffGraph.addNode(use)
    diffGraph.addEdge(use, typeNode, EdgeTypes.AST)
    diffGraph.addEdge(use, name, EdgeTypes.AST)
    alias match {
      case Some(node) => diffGraph.addEdge(use, node, EdgeTypes.AST)
      case None       =>
    }
    use
  }

  def createStmtNamespace(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    //val attributes = getJsonObject("attributes", json).get
    val name = getJsonObject("name", json).get match {
      case name: Map[String, Any] => nameNodeToNameString(name)
    }
    val stmts = getJsonList("stmts", json)
    val namespace = nodes
      .NewNamespaceBlock()
      .name(name)
      .fullName(name)
      .order(childNum)
    diffGraph.addNode(namespace)
    diffGraph.addEdge(namespace, createCodeBlock(stmts, 0), EdgeTypes.AST)
    namespace
  }

  def createExprShellExec(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val attributes = getJsonObject("attributes", json).get
    val parts = create(getJsonList("parts", json))
    val call = nodes
      .NewCall()
      .code(s"`${toCode(parts).mkString(" ; ")}`")
      .name("ShellExec")
      .lineNumber(getLineStart(attributes))
      .order(childNum)
    diffGraph.addNode(call)
    parts.foreach(node => diffGraph.addEdge(call, node, EdgeTypes.AST))
    call
  }

  def createExprInstanceOf(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val attributes = getJsonObject("attributes", json).get
    val expr: nodes.NewNode = create(getJsonObject("expr", json).get, 0)
    val eClassJson = getJsonObject("class", json).get
    val eClass = getJsonAtom[String]("nodeType", eClassJson).get match {
      case "Name" =>
        nodes
          .NewLiteral()
          .code(nameNodeToNameString(eClassJson))
          .typeFullName("classIdentifier")
      case _ => create(eClassJson, 1)
    }
    val call = nodes
      .NewCall()
      .code(s"${toCode(expr)} instanceof ${toCode(eClass)}")
      .name("instanceof")
      .lineNumber(getLineStart(attributes))
      .order(childNum)
    diffGraph.addNode(call)
    diffGraph.addEdge(call, expr, EdgeTypes.AST)
    diffGraph.addEdge(call, eClass, EdgeTypes.AST)
    call
  }

  def createExprAssignOp(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val attributes = getJsonObject("attributes", json).get
    val variable = create(getJsonObject("var", json).get, 0)
    val expr = create(getJsonObject("expr", json).get, 1)
    val (code, _) = getJsonAtom[String]("nodeType", json).get
      .substring("Expr_AssignOp_".length) match {
      case "Plus"       => ("+=", s"AssignOp_Plus")
      case "Minus"      => ("-=", s"AssignOp_Minus")
      case "Mul"        => ("*=", s"AssignOp_Mul")
      case "Div"        => ("/=", s"AssignOp_Div")
      case "Mod"        => ("%=", s"AssignOp_Mod")
      case "Concat"     => (".=", "AssignOp_Concat")
      case "BitwiseAnd" => ("&=", "AssignOp_BitwiseAnd")
      case "BitwiseOr"  => ("|=", "AssignOp_BitwiseOr")
      case "BitwiseXor" => ("^=", "AssignOp_BitwiseXor")
      case "ShiftLeft"  => ("<<=", "AssignOp_ShiftLeft")
      case "ShiftRight" => ("<<=", "AssignOp_ShiftRight")
      case "Coalesce"   => ("??=", "AssignOp_Coalesce")
    }
    val assignOp = nodes
      .NewCall()
      .code(s"${toCode(variable)} $code ${toCode(expr)}")
      .name(code)
      .lineNumber(getLineStart(attributes))
      .order(childNum)
    diffGraph.addNode(assignOp)
    diffGraph.addEdge(assignOp, variable, EdgeTypes.AST)
    diffGraph.addEdge(assignOp, expr, EdgeTypes.AST)
    assignOp
  }

  def createExprNew(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val attributes = getJsonObject("attributes", json).get
    val className = getJsonObject("class", json).get match {
      case name: Map[String, Any] =>
        try {
          nameNodeToNameString(name)
        } catch {
          case _: AssertionError => "$VARIABLE"
        }
    }
    val stringLiteral = create(
      Map(
        "nodeType" -> "Scalar_String",
        "value" -> className,
        "attributes" -> Map(
          "startLine" -> getLineStart(attributes).get.toDouble
        )
      ),
      0)
    val args = create(getJsonList("args", json), 1)
    val newStmt = nodes
      .NewCall()
      .code(s"new ${toCode(stringLiteral)}(${toCode(args).mkString(",")})")
      .name("new")
      .order(childNum)
      .lineNumber(getLineStart(attributes))
    diffGraph.addNode(newStmt)

    diffGraph.addEdge(newStmt, stringLiteral, EdgeTypes.AST)
    args.foreach(node => diffGraph.addEdge(newStmt, node, EdgeTypes.AST))
    newStmt
  }

  def createExprPropertyFetch(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val attributes = getJsonObject("attributes", json).get
    val variable = create(getJsonObject("var", json).get, 0)
    val name = create(getJsonObject("name", json).get, 1)
    val fetch = nodes
      .NewCall()
      .code(s"${toCode(variable)}->${toCode(name)}")
      .name("->")
      .order(childNum)
      .lineNumber(getLineStart(attributes))
    diffGraph.addNode(fetch)
    diffGraph.addEdge(fetch, variable, EdgeTypes.AST)
    diffGraph.addEdge(fetch, name, EdgeTypes.AST)
    fetch
  }

  def createExprAssign(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val attributes = getJsonObject("attributes", json).get
    val variable = create(getJsonObject("var", json).get, 0)
    val expr = create(getJsonObject("expr", json).get, 1)
    val assign = nodes
      .NewCall()
      .code(s"${toCode(variable)} = ${toCode(expr)}")
      .name("assign")
      .order(childNum)
      .lineNumber(getLineStart(attributes))

    diffGraph.addNode(assign)
    diffGraph.addEdge(assign, variable, EdgeTypes.AST)
    diffGraph.addEdge(assign, expr, EdgeTypes.AST)
    assign
  }

  def createStmtClassMethod(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val attributes = getJsonObject("attributes", json).get
    //val returnType = getJsonAtom[String]("returnType", json)
    val name = getJsonObject("name", json).get match {
      case name: Map[String, Any] => getJsonAtom[String]("name", name).get
    }
    val flags = translateFlag(getJsonAtom[Double]("flags", json).get)
    val params: Seq[nodes.NewNode] = create(getJsonList("params", json))
    val stmts = getJsonList("stmts", json)
    val method = nodes
      .NewMethod()
      .code(s"$name(${toCode(params).mkString(",")})")
      .name(name)
      .fullName(name)
      .isExternal(flags.contains("public"))
      .lineNumber(getLineStart(attributes))
      .lineNumberEnd(getLineEnd(attributes))
      .order(childNum)
    diffGraph.addNode(method)
    params.foreach(param => diffGraph.addEdge(method, param, EdgeTypes.AST))
    diffGraph.addEdge(method,
                      createCodeBlock(stmts, params.length),
                      EdgeTypes.AST)
    method
  }

  def createStmtProperty(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    // flags 9.0 -> public static, 1.0 -> public, 4.0 -> private, 12.0 -> private static
    //val flags = translateFlag(getJsonAtom[Double]("flags", json).get)
    val property = getJsonList("props", json)
    assert(property.length == 1)
    val (attributes, name, _) = property match {
      case prop :: Nil =>
        prop match {
          case prop: Map[String, Any] =>
            val attributes = getJsonObject("attributes", prop).get
            val name = getJsonObject("name", prop).get match {
              case ident: Map[String, Any] =>
                getJsonAtom[String]("name", ident).get
            }
            val default = getJsonObject("default", prop)
            (attributes, name, default)
          case unexpectedPattern =>
            throw new RuntimeException(
              s"unexpected pattern of $unexpectedPattern")
        }
      case unexpectedPattern =>
        throw new RuntimeException(s"unexpected pattern of $unexpectedPattern")
    }
    val node = nodes
      .NewIdentifier()
      .name(name)
      .order(childNum)
      .typeFullName(name)
      .lineNumber(getLineStart(attributes))
    diffGraph.addNode(node)
    node
  }

  def createStmtClass(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    //val attributes = getJsonObject("attributes", json).get
    val name = getJsonAtom[String]("name", getJsonObject("name", json).get).get
    //val flags = translateFlag(getJsonAtom[Double]("flags", json).get)
    val implements = getJsonAtom[List[_]]("implements", json).get
    assert(implements.isEmpty,
           "implements keyword in class creation is currently not supported")
    val ext = getJsonObject("extends", json) match {
      case Some(name) => List(nameNodeToNameString(name))
      case None       => List()
    }
    val stmts = getJsonList("stmts", json)
    val classType = nodes
      .NewTypeDecl()
      .name(name)
      .fullName(name)
      .isExternal(true)
      .inheritsFromTypeFullName(ext)
      .order(childNum)
    diffGraph.addNode(classType)
    diffGraph.addEdge(classType, createCodeBlock(stmts, 0), EdgeTypes.AST)
    classType
  }

  def createExprConstFetch(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val attributes = getJsonObject("attributes", json).get
    val name = getJsonObject("name", json) match {
      case name: Option[Map[String, Any]] =>
        getJsonAtom[List[_]]("parts", name.get).get match {
          case sub :: Nil =>
            sub match {
              case x: String => x
            }
        }
    }
    val node = nodes
      .NewLiteral()
      .code(s"$name")
      .typeFullName("constant")
      .lineNumber(getLineStart(attributes))
      .order(childNum)
    diffGraph.addNode(node)
    node
  }

  def createStmtBreak(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val attributes = getJsonObject("attributes", json).get
    val node = nodes
      .NewControlStructure()
      .code("break")
      .parserTypeName("break")
      .lineNumber(getLineStart(attributes))
      .order(childNum)
    diffGraph.addNode(node)
    node
  }

  def createStmtCase(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val attributes = getJsonObject("attributes", json).get
    val trueJson: Map[String, Any] =
      Map(
        "nodeType" -> "Expr_ConstFetch",
        "name" -> Map("nodeType" -> "Name", "parts" -> List("true")),
        "attributes" -> Map(
          "startLine" -> getLineStart(attributes).get.toDouble)
      )
    val condition = create(getJsonObject("cond", json).getOrElse(trueJson), 0)
    val stmt = nodes
      .NewControlStructure()
      .code(s"case(${toCode(condition)}):")
      .parserTypeName("case")
      .lineNumber(getLineStart(attributes))
      .order(childNum)
    diffGraph.addNode(stmt)
    diffGraph.addEdge(stmt, condition, EdgeTypes.AST)
    diffGraph.addEdge(stmt,
                      createCodeBlock(getJsonList("stmts", json), 1),
                      EdgeTypes.AST)
    stmt
  }

  def createStmtSwitch(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val attributes = getJsonObject("attributes", json).get
    val cond = create(getJsonObject("cond", json).get, 0)
    val switch = nodes
      .NewControlStructure()
      .code(s"switch(${toCode(cond)})")
      .parserTypeName("switch")
      .lineNumber(getLineStart(attributes))
      .order(childNum)
    diffGraph.addNode(switch)
    diffGraph.addEdge(switch, cond, EdgeTypes.AST)
    create(getJsonList("cases", json), 1).foreach { node =>
      diffGraph.addEdge(switch, node, EdgeTypes.AST)
    }
    switch
  }

  def createStmtReturn(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val attributes = getJsonObject("attributes", json).get
    val expr = create(getJsonObject("expr", json).get, 0)
    val ret = nodes
      .NewReturn()
      .code(s"return ${toCode(expr)}")
      .lineNumber(getLineStart(attributes))
      .order(childNum)
    diffGraph.addNode(ret)
    diffGraph.addEdge(ret, expr, EdgeTypes.AST)
    ret
  }

  def createParam(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val default = getJsonObject("default", json) match {
      case None => None
      case Some(_) => None
    }
    assert(default.isEmpty,
           s"default should be None but is ${default.getClass.toString}")
    val ptype = getJsonObject("type", json)
    val name = getJsonObject("var", json).get match {
      case pvar: Map[String, Any] => getJsonAtom[String]("name", pvar).get
    }
    assert(getJsonDouble("flags", json).contains(0.0))
    val node = nodes
      .NewMethodParameterIn()
      .code("$" + name)
      .order(childNum)
      .name(name)
      .typeFullName(ptype match {
        case Some(x) =>
          try {
            getJsonAtom[String]("name", x).get
          } catch {
            case _: RuntimeException => nameNodeToNameString(x)
          }
        case None => ""
      })
    diffGraph.addNode(node)
    node
  }

  def createStmtFunction(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val stmts = getJsonList("stmts", json)
    val attributes = getJsonObject("attributes", json).get
    val params = create(getJsonList("params", json))
    val name = getJsonObject("name", json).get match {
      case x: Map[String, Any] => getJsonAtom[String]("name", x).get
    }
    val function = nodes
      .NewMethod()
      .code(s"$name(${toCode(params).mkString(",")})")
      .name(name)
      .fullName(name)
      .isExternal(true)
      .lineNumber(getLineStart(attributes))
      .lineNumberEnd(getLineStart(attributes))
      .order(childNum)
    diffGraph.addNode(function)
    params.foreach(param => diffGraph.addEdge(function, param, EdgeTypes.AST))
    diffGraph.addEdge(function,
                      createCodeBlock(stmts, params.length),
                      EdgeTypes.AST)
    function
  }

  def createStmtExpression(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    json.getOrElse("expr", missingKey("expr", json)) match {
      case expr: Map[String, Any] => create(expr, childNum)
    }
  }

  def createStmtEcho(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val attributes = getJsonObject("attributes", json).get
    val exprs = createCodeBlock(getJsonList("exprs", json), 0)
    val call = nodes
      .NewCall()
      .code(s"echo ${toCode(exprs)}")
      .name("echo")
      .order(childNum)
      .lineNumber(getLineStart(attributes))
    diffGraph.addNode(call)

    diffGraph.addEdge(call, exprs, EdgeTypes.AST)
    call
  }

  def createStmtElseIf(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val attributes = getJsonObject("attributes", json).get
    val cond = create(getJsonObject("cond", json).get, 0)
    val stmts = createCodeBlock(getJsonList("stmts", json), 1)
    val call = nodes
      .NewControlStructure()
      .code(s"elseif(${toCode(cond)})")
      .parserTypeName("elseif")
      .lineNumber(getLineStart(attributes))
      .order(childNum)
    diffGraph.addNode(call)
    diffGraph.addEdge(call, cond, EdgeTypes.AST)
    diffGraph.addEdge(call, stmts, EdgeTypes.AST)
    call
  }

  def createStmtIf(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val attributes = getJsonObject("attributes", json).get
    val cond = create(getJsonObject("cond", json).get, 0)
    val stmts = createCodeBlock(getJsonList("stmts", json), 1)
    val elseifs = create(getJsonList("elseifs", json), 2)
    val elseNode = getJsonObject("else", json) match {
      case Some(json) => Some(create(json, 2 + elseifs.length))
      case None       => None
    }
    val call = nodes
      .NewControlStructure()
      .code(s"if(${toCode(cond)})")
      .parserTypeName("if")
      .lineNumber(getLineStart(attributes))
      .order(childNum)
    diffGraph.addNode(call)
    // adding the condition
    diffGraph.addEdge(call, cond, EdgeTypes.AST)
    // adding the true branch
    diffGraph.addEdge(call, stmts, EdgeTypes.AST)
    // adding the elseif branch
    elseifs.foreach(node => diffGraph.addEdge(call, node, EdgeTypes.AST))
    // adding the else branch
    elseNode match {
      case Some(x) => diffGraph.addEdge(call, x, EdgeTypes.AST)
      case None    =>
    }
    call
  }

  def createExprFuncCall(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val name: String = json.getOrElse("name", missingKey("name", json)) match {
      case name: Map[String, Any] =>
        try {
          nameNodeToNameString(name)
        } catch {
          case _: AssertionError => "VARIABLE"
        }
    }
    val args = create(getJsonList("args", json))
    val attributes = getJsonObject("attributes", json).get
    val call = nodes
      .NewCall()
      .code(s"$name(${toCode(args).mkString(",")}")
      .name(name)
      .order(childNum)
      .argumentIndex(args.length)
      .lineNumber(getLineStart(attributes))
    diffGraph.addNode(call)
    args.foreach(node => diffGraph.addEdge(call, node, EdgeTypes.AST))
    call
  }

  def createExprMethodCall(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val name: String = getJsonObject("name", json).get match {
      case name: Map[String, Any] => getJsonAtom[String]("name", name).get
    }
    val obj = create(getJsonObject("var", json).get, 0)
    val args = create(getJsonList("args", json), 1)
    val attributes = getJsonObject("attributes", json).get
    val call = nodes
      .NewCall()
      .code(s"${toCode(obj)}->$name(${toCode(args).mkString(",")})")
      .name(name)
      .order(childNum)
      .lineNumber(getLineStart(attributes))
    diffGraph.addNode(call)
    args.foreach(node => diffGraph.addEdge(call, node, EdgeTypes.AST))
    call
  }

  def createExprStaticCall(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val name: String = getJsonObject("name", json).get match {
      case name: Map[String, Any] => getJsonAtom[String]("name", name).get
    }
    val obj = getJsonObject("class", json).get match {
      case name: Map[String, Any] =>
        try {
          nameNodeToNameString(name)
        } catch {
          case _: AssertionError => "WEIRDNAME"
        }
    }
    val args = create(getJsonList("args", json))
    val attributes = getJsonObject("attributes", json).get
    val call = nodes
      .NewCall()
      .code(s"$obj::$name(${toCode(args).mkString(",")}")
      .name(name)
      .order(childNum)
      .lineNumber(getLineStart(attributes))
    diffGraph.addNode(call)
    args.foreach(node => diffGraph.addEdge(call, node, EdgeTypes.AST))
    call
  }

  def createExprBinaryOp(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val lhs = create(getJsonObject("left", json).get, 0)
    val rhs = create(getJsonObject("right", json).get, 1)
    val (code, _) = getJsonAtom[String]("nodeType", json).get
      .substring("Expr_BinaryOp_".length) match {
      case "Plus"           => ("+", "BinaryOp_Plus")
      case "Minus"          => ("-", "BinaryOp_Minus")
      case "Div"            => ("/", "BinaryOp_Div")
      case "Mod"            => ("%", "BinaryOp_Mod")
      case "Mul"            => ("*", "BinaryOp_Mul")
      case "Equal"          => ("==", "BinaryOp_Equal")
      case "Pow"            => ("**", "BinaryOp_Equal")
      case "BitwiseAnd"     => ("&", "BinaryOp_BitwiseAnd")
      case "BitwiseOr"      => ("|", "BinaryOp_BitwiseOr")
      case "ShiftLeft"      => ("<<", "BinaryOp_ShiftLeft")
      case "ShiftRight"     => (">>", "BinaryOp_ShiftRight")
      case "Concat"         => (".", "BinaryOp_Concat")
      case "Coalesce"       => ("??", "BinaryOp_Coalesce")
      case "Identical"      => ("===", "BinaryOp_Identical")
      case "NotEqual"       => ("!=", "BinaryOp_NotEqual")
      case "NotIdentical"   => ("!==", "BinaryOp_NotIdentical")
      case "Smaller"        => ("<", "BinaryOp_Smaller")
      case "Greater"        => (">", "BinaryOp_Greater")
      case "SmallerOrEqual" => ("<=", "BinaryOp_SmallerOrEqual")
      case "GreaterOrEqual" => (">=", "BinaryOp_GreaterOrEqual")
      case "Spaceship"      => ("<=>", "BinaryOp_Spaceship")
      case "LogicalAnd"     => ("&&", "BinaryOp_LogicalAnd")
      case "LogicalOr"      => ("||", "BinaryOp_LogicalOr")
      case "BooleanAnd"     => ("and", "BinaryOp_BooleanAnd")
      case "BooleanOr"      => ("or", "BinaryOp_BooleanOr")
      case "LogicalXor"     => ("xor", "BinaryOp_LogicalXor")
    }
    val attributes = getJsonObject("attributes", json).get
    val call = nodes
      .NewCall()
      .code(s"${toCode(lhs)} $code ${toCode(rhs)}")
      .name(code)
      .order(childNum)
      .argumentIndex(2)
      .lineNumber(getLineStart(attributes))

    diffGraph.addNode(call)
    diffGraph.addEdge(call, lhs, EdgeTypes.AST)
    diffGraph.addEdge(call, rhs, EdgeTypes.AST)
    call
  }

  def createExprUnaryOp(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val attributes = getJsonObject("attributes", json).get
    val (code, _) = getJsonAtom[String]("nodeType", json).get
      .substring("Expr_".length) match {
      case "BooleanNot"    => ("!", "BooleanNot")
      case "UnaryMinus"    => ("-", "UnaryMinus")
      case "UnaryPlus"     => ("+", "UnaryPlus")
      case "BitwiseNot"    => ("~", "BitwiseNot")
      case "PreInc"        => ("--", "PreInc")
      case "PostInc"       => ("--", "PostInc")
      case "PreDec"        => ("++", "PreDec")
      case "PostDec"       => ("++", "PostDec")
      case "ErrorSuppress" => ("@", "ErrorSuppress")
    }
    val expr = create(json.get("expr") match {
      case Some(x) =>
        x match {
          case expr: Map[String, Any] => expr
        }
      case None => getJsonObject("var", json).get
    }, 0)
    val op = nodes
      .NewCall()
      .code(s"$code${toCode(expr)}")
      .name(code)
      .lineNumber(getLineStart(attributes))
      .order(childNum)
    diffGraph.addNode(op)
    diffGraph.addEdge(op, expr, EdgeTypes.AST)
    op
  }

  def createExprVariable(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val name = getJsonAtom[String]("name", json).get
    val attributes = getJsonObject("attributes", json).get
    val node = nodes
      .NewIdentifier()
      .code("$" + name)
      .name(name)
      .order(childNum)
      .lineNumber(getLineStart(attributes))
    diffGraph.addNode(node)
    node
  }

  def createArg(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    create(getJsonObject("value", json).get, childNum)
  }

  def createScalar(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val stype =
      getJsonAtom[String]("nodeType", json).get.substring("Scalar_".length)
    if (stype == "Encapsed") {
      createScalarEncapsedString(json, childNum)
    } else {
      val value = getJsonAtom[Any]("value", json).get
      val attributes = getJsonObject("attributes", json).get
      val node = nodes
        .NewLiteral()
        .typeFullName(stype)
        .code(s"$value")
        .order(childNum)
        .lineNumber(getLineStart(attributes))
      diffGraph.addNode(node)
      node
    }
  }

  def createScalarEncapsedString(json: Map[String, Any], childNum: Integer)(
      implicit diffGraph: DiffGraph.Builder): nodes.NewNode = {
    val attributes = getJsonObject("attributes", json).get
    val parts = create(getJsonList("parts", json))
    val node = nodes
      .NewCall()
      .code("\"" + toCode(parts).mkString("") + "\"")
      .name("EncapsedString")
      .lineNumber(getLineStart(attributes))
      .order(childNum)
    diffGraph.addNode(node)
    parts.foreach(child => diffGraph.addEdge(node, child, EdgeTypes.AST))
    node
  }

}
