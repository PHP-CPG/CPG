package io.joern.bytecode.passes.dataDependencyPasses

import io.joern.bytecode.passes.dataDependencyPasses.WriteOps.{
  IdentifierWrite,
  requiredParamsSatisfied
}
import io.joern.bytecode.util.extensions.NodeExtension._
import io.joern.bytecode.util.implicits.OneableSeq
import io.joern.reporting.{ReportableError, Reporting}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, nodes}
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool, ParallelCpgPass}
import io.shiftleft.semanticcpg.language._

import scala.collection.immutable.{Map, Set}
import scala.jdk.CollectionConverters.IteratorHasAsScala

// this could be outsourced into a file to make this pass generic and dependent on a definition file
// that can be adjusted for each CPG
object WriteOps {

  /**
    *
    * @param operation           the name of the bytecode operation
    * @param position            position of the identifier
    * @param reading
    * @param required_parameters only use this definition, if the required parameters (pos: Int, value: String) match
    */
  case class IdentifierWrite(operation: String,
                             position: Option[Int],
                             reading: Boolean,
                             required_parameters: Option[Map[Int, String]] =
                               None)

  val writes: Set[IdentifierWrite] = Set(
    IdentifierWrite("ASSIGN", Some(0), reading = false),
    IdentifierWrite("ASSIGN_OP", Some(1), reading = true),
    IdentifierWrite("BIND_GLOBAL", Some(0), reading = false),
    IdentifierWrite("FE_FETCH_R", Some(1), reading = false),
    IdentifierWrite("FE_FREE", Some(0), reading = true),
    IdentifierWrite("FREE", Some(0), reading = true),
    IdentifierWrite("=", Some(0), reading = false),
    IdentifierWrite("ASSIGN_DIM", Some(0), reading = true),
    IdentifierWrite("ASSIGN_REF_2", Some(0), reading = false),
    IdentifierWrite("ASSIGN_REF_3", Some(1), reading = false),
    IdentifierWrite("ASSIGN_DIM_OP", Some(1), reading = true),
    IdentifierWrite("PRE_INC", Some(0), reading = true),
    IdentifierWrite("PRE_DEC", Some(0), reading = true),
    IdentifierWrite("POST_INC", Some(0), reading = true),
    IdentifierWrite("POST_DEC", Some(0), reading = true),
    IdentifierWrite("ASSIGN_OBJ",
                    Some(1),
                    reading = false,
                    required_parameters = Some(Map(0 -> "THIS")))
    //IdentifierWrite("UNSET_VAR",Some(1), reading = false)
  )

  /**
    * Does the provided call satisfy the required parameters? If the parameters are None it returns true.
    *
    * @param call                : the call in question
    * @param required_parameters see above
    */
  def requiredParamsSatisfied(
      call: nodes.Call,
      required_parameters: Option[Map[Int, String]]): Boolean = {
    if (call.name == "ASSIGN_OBJ" && call.astChildren
          .order(1)
          .isLiteral
          .isEmpty) return false
    required_parameters match {
      case None => true
      case Some(value) =>
        value.forall(x =>
          call.argument.filter(_.order == x._1).isLiteral.headOption match {
            case Some(value) => value.code == x._2
            case None        => false
        })
    }
  }
}

class DataDependencyPass(methods: Seq[nodes.Method],
                         cpg: Cpg,
                         keyPool: IntervalKeyPool)
    extends ParallelCpgPass[nodes.Method](cpg: Cpg,
                                          keyPools =
                                            Some(keyPool.split(methods.size)))
    with Reporting {

  val PREFIX_IDENTIFIER: String = "IDENTIFIER-"
  val PREFIX_THISLITERAL: String = "THISLITERAL-"
  case class PseudoIdentifier(name: String, position: Int) {
    def rawName: String = name match {
      case x if x.startsWith(PREFIX_IDENTIFIER) =>
        x.splitAt(PREFIX_IDENTIFIER.length)._2
      case x if x.startsWith(PREFIX_THISLITERAL) =>
        x.splitAt(PREFIX_THISLITERAL.length)._2
    }
  }
  type DDGCalcSet = Set[(PseudoIdentifier, Long)]

  override val name = "DataDependencyPass"

  override def partIterator: Iterator[Method] = methods.iterator

  override def runOnPart(part: Method): Iterator[DiffGraph] = {
    implicit val diffGraph: DiffGraph.Builder = DiffGraph.newBuilder
    try {

      // here we get all the cfg nodes in depth first spanning tree order which is supposed to optimize runtime
      val cfgNodes = generateDepthFirstSpanningTreeOrder(part)
      // here we generate the initial out set and the final gen, and kill sets for each cfg node
      val (gen, out, kill) = genOutGenKillPreparation(cfgNodes)
      // here we calculate the final in and out set for each cfg node
      val in = inOutSetCalculation(cfgNodes, out, gen, kill)
      //prettyPrintMap("in", in)
      // here we create the reaching definition edges based on the in set finalized before
      createReachingEdge(in)
      //handleOpDataCalls(cfgNodes)
    } catch {
      case err: ReportableError => reportError(err)
      case thr: Throwable =>
        reportError(part.filename,
                    "NA",
                    part.fullName,
                    "NA",
                    thr.toString + thr.getStackTrace.mkString("\n"),
                    thr)
    }
    Iterator(diffGraph.build())
  }

  /** prepare the sets for out gen and kill for each cfg node
    *
    * This is done by checking whether the given cfg node is a known writing call and if so
    * gen and kill are set to Set((Indentifier, call))
    *
    * @param cfgNodes - all the cfg nodes of a given method
    */
  private def genOutGenKillPreparation(
      cfgNodes: List[nodes.CfgNode]): (Map[nodes.CfgNode, DDGCalcSet],
                                       Map[nodes.CfgNode, DDGCalcSet],
                                       Map[nodes.CfgNode, DDGCalcSet]) = {
    var out: Map[nodes.CfgNode, DDGCalcSet] = Map()
    var gen: Map[nodes.CfgNode, DDGCalcSet] = Map()
    var kill: Map[nodes.CfgNode, DDGCalcSet] = Map()
    cfgNodes.foreach {
      case call: nodes.Call =>
        WriteOps.writes.find(_.operation == call.name) match {
          case Some(IdentifierWrite(_, position, _, required_parameters))
              if requiredParamsSatisfied(call, required_parameters) =>
            try {
              val pos: Int = position match {
                case Some(value) => value
                case None =>
                  val parentMethod = call.getParentMethod.get
                  throw ReportableError(
                    parentMethod.filename,
                    call.lineNumber.getOrElse(Integer.getInteger("-1")).toInt,
                    parentMethod.name,
                    call.code,
                    "no expected position of identifier write"
                  )
              }
              val identifier = call.name match {
                case "ASSIGN_OBJ" =>
                  assert(
                    call.astChildren
                      .order(0)
                      .isLiteral
                      .exists(_.code == "THIS"))
                  val tmp_literal = call.astChildren.order(1).isLiteral.l.head
                  PseudoIdentifier(PREFIX_THISLITERAL + tmp_literal.code, 1)
                case _ =>
                  val tmp_identifier =
                    call.astChildren
                      .order(pos)
                      .head
                      .asInstanceOf[nodes.Identifier]

                  PseudoIdentifier(PREFIX_IDENTIFIER + tmp_identifier.name,
                                   tmp_identifier.order)
              }

              val newElement: DDGCalcSet =
                Set((identifier, call.id()))
              gen = gen + (call -> (gen.getOrElse(
                call,
                Set[(PseudoIdentifier, Long)]()) ++ newElement))
              kill = kill + (call -> (kill.getOrElse(call, Set()) ++ newElement))
              out = out + (call -> (out.getOrElse(call, Set()) ++ newElement))
            } catch {
              case x: Throwable =>
                val parentMethod = call.getParentMethod.get
                throw ReportableError(
                  parentMethod.filename,
                  call.lineNumber.getOrElse(Integer.getInteger("-1")).toInt,
                  parentMethod.name,
                  call.code,
                  x.getMessage)
            }
          case _ =>
            gen = gen + (call -> Set())
            kill = kill + (call -> Set())
            out = out + (call -> Set())
        }
      case node: nodes.CfgNode =>
        gen = gen + (node -> Set())
        kill = kill + (node -> Set())
        out = out + (node -> Set())
    }
    (out, gen, kill)
  }

  /**
    *
    * Algorithm based on Compiler Bau II by Alfred V. Aho, Ravi Sethis, and Jeffrey D. Ullmann
    * Chapter 10.6 Algorithm 10.2
    *
    * @param cfgNodes - all the cfg nodes of a given method
    * @param out      - the out set for each cfgNode to be generated/worked on
    * @param gen      - the gen set for each cfgNode to be worked with
    * @param kill     - the kill set for each cfgNode to be worked with
    */
  private def inOutSetCalculation(
      cfgNodes: List[nodes.CfgNode],
      out: Map[nodes.CfgNode, DDGCalcSet],
      gen: Map[nodes.CfgNode, DDGCalcSet],
      kill: Map[nodes.CfgNode, DDGCalcSet]): Map[nodes.CfgNode, DDGCalcSet] = {
    var counter = cfgNodes.length
    var currentIn: Map[nodes.CfgNode, DDGCalcSet] =
      Map()
    var currentOut = out
    var change: Boolean = true
    while (change) {
      if (counter == 0) {
        val parentMethod = cfgNodes.head.getParentMethod.get
        throw ReportableError(
          parentMethod.filename,
          cfgNodes.head.lineNumber.getOrElse(Integer.getInteger("-1")).toInt,
          parentMethod.fullName,
          "NA",
          "too many iterations to generate in-out-set"
        )
      } else {
        counter -= 1
      }
      change = false
      cfgNodes.foreach { call =>
        val toUnioninSets = call.cfgPrev
          .map(incoming => currentOut(incoming))
        val mInUnionSet =
          new collection.mutable.HashSet[(PseudoIdentifier, Long)]()
        toUnioninSets.foreach(mInUnionSet ++= _)
        currentIn = currentIn + (call -> mInUnionSet.toSet)
        //prettyPrintSet("new incoming", call, currentIn)
        val oldOut = currentOut(call)
        //prettyPrintSet("old outgoing", call, currentOut)
        val killedVariableList = kill(call).toList
        assert(killedVariableList.length <= 1)
        var newOut = Set[(PseudoIdentifier, Long)]()
        // if there is no variable killed
        if (killedVariableList.isEmpty) {
          // then all incoming variables plus the generated variables are outgoing
          newOut = gen(call) union currentIn(call)
        } else {
          // if stuff is killed then we have to filter the incoming set for those variables before
          // creating the union with the generated variables
          newOut = gen(call) union currentIn(call).filter(
            _._1.name != killedVariableList.head._1.name)
        }
        if ((newOut &~ oldOut).nonEmpty) {
          change = true
          currentOut = currentOut + (call -> newOut)
        }
        // after updating the set for the call is either the same or larger
        assert(currentOut(call).size >= oldOut.size)
        // the size has to be the size of the newOut as it is either the same or has been larger
        assert(currentOut(call).size == newOut.size)
      }
    }
    currentIn
  }

  def addReachingEdge(
      from: nodes.CfgNode,
      to: nodes.CfgNode,
      identifier: String)(implicit diffGraph: DiffGraph.Builder): Unit = {
    diffGraph.addEdge(from,
                      to,
                      EdgeTypes.REACHING_DEF,
                      List(("VARIABLE", identifier)))
  }

  def getUsedIdentifiers(call: nodes.Call): Set[PseudoIdentifier] = {
    call.name match {
      // if we have an = it means on the right had side is a call and thus only the lhs has a valid identifier child
      case "=" =>
        assert(
          call.astChildren.order(1).next().isInstanceOf[nodes.Call],
          s"the rhs of a = should be a call but we encountered ${call.code}")
        val tmp_identifier =
          call.astChildren.order(0).next().asInstanceOf[nodes.Identifier]
        Set(
          PseudoIdentifier(PREFIX_IDENTIFIER + tmp_identifier.name,
                           tmp_identifier.order))
      case "ASSIGN_OBJ"
          if call.astChildren
            .order(0)
            .isLiteral
            .exists(_.code == "THIS") && call.astChildren
            .order(1)
            .isLiteral
            .nonEmpty =>
        Set(PseudoIdentifier(
          PREFIX_THISLITERAL + call.astChildren.order(1).isLiteral.l.head.code,
          1))
      case "FETCH_OBJ_R"
          if call.astChildren
            .order(0)
            .isLiteral
            .exists(_.code == "THIS") && call.astChildren
            .order(1)
            .isLiteral
            .nonEmpty =>
        Set(PseudoIdentifier(
          PREFIX_THISLITERAL + call.astChildren.order(1).isLiteral.l.head.code,
          1))
      case _ =>
        call.astMinusRoot.isIdentifier
          .map(x => PseudoIdentifier(PREFIX_IDENTIFIER + x.name, x.order))
          .toSet
          .toSet
    }
  }

  /** Based on a calculated in set create corresponding reaching edges
    *
    * @param in        - a map giving you the incoming identifier,definingCall pairs for a given call
    * @param diffGraph - the diffGraph to generate which is later merged into a preexisting cpg
    */
  def createReachingEdge(in: Map[nodes.CfgNode, DDGCalcSet])(
      implicit diffGraph: DiffGraph.Builder): Unit = {
    in.filter(_._1.isInstanceOf[nodes.Call]).foreach {
      case (callNode, set) =>
        // this are the identifiers used in the current expression
        val usedIdentifiers =
          getUsedIdentifiers(callNode.asInstanceOf[nodes.Call])
        // this are the identifier used in the current expression intersected with the incoming identifiers
        val inIntersection =
          usedIdentifiers.filter(ident =>
            set.exists(elem => elem._1.name == ident.name))
        // this is the write op the current callNode corresponds with
        val writeOp = WriteOps.writes.find(
          _.operation == callNode.asInstanceOf[nodes.Call].name)
        // now we go over the intersecting identifier
        inIntersection.foreach { identifier =>
          // and based on whether we have a write op we create reaching edges
          writeOp match {
            // if it is a write op call we have to check that it is either a reading write op or the identifier is not at the writing position
            case Some(IdentifierWrite(_, position, reading, _)) =>
              if (reading || identifier.position != position.get) {
                val definitions =
                  set.filter(elem => elem._1.name == identifier.name)
                definitions.foreach {
                  case (identifier, definedAt) =>
                    addReachingEdge(
                      cpg.all.id(definedAt).collectAll[nodes.Call].l.one,
                      callNode,
                      identifier.rawName)
                }
              } else {}
            // if it is not a write op we can simply add the edge
            case None =>
              val definitions =
                set.filter(elem => elem._1.name == identifier.name)
              definitions.foreach {
                case (identifier, definedAt) =>
                  addReachingEdge(
                    cpg.all.id(definedAt).collectAll[nodes.Call].l.one,
                    callNode,
                    identifier.rawName)
              }
          }

        }
    }
  }

  /** returns all cfg nodes of a method in depth first spanning tree order
    *
    * algorithm based on Compiler Bau II by Alfred V. Aho, Ravi Sethis, and Jeffrey D. Ullmann
    * Chapter 10.9, Algorithm 10.14
    *
    * @param start the start of the cfg i.e., the method itself
    * @return returns the list of all contained cfg nodes in depth first spanning tree order
    */
  def generateDepthFirstSpanningTreeOrder(
      start: Method): List[nodes.CfgNode] = {
    val order: collection.mutable.ListBuffer[(Int, nodes.CfgNode)] =
      collection.mutable.ListBuffer()
    val cfgNodeCount = start.ast.isCfgNode.l.length
    def populateOrder(current: nodes.CfgNode, i: Int, recursionDepth: Int = 0)(
        implicit visited: collection.mutable.Set[nodes.CfgNode]): Int = {
      assert(
        recursionDepth <= cfgNodeCount,
        s"the recursion depth exceeded the maximum amount of cfg nodes of $cfgNodeCount")
      var currentI = i
      current._cfgOut.asScala.foreach { next =>
        if (!visited.contains(next.asInstanceOf[nodes.CfgNode])) {
          visited.addOne(next.asInstanceOf[nodes.CfgNode])
          currentI = populateOrder(next.asInstanceOf[nodes.CfgNode],
                                   currentI,
                                   recursionDepth + 1)
        }
      }
      order.append((currentI, current))
      currentI - 1
    }
    populateOrder(start, cfgNodeCount)(collection.mutable.Set())
    augmentDFSTOrderList(start, order.sortBy(_._1).map(_._2).toList)
  }

  def augmentDFSTOrderList(start: Method,
                           dfst: List[nodes.CfgNode]): List[nodes.CfgNode] = {
    val dfstSet = dfst.toSet
    dfst ++ start.ast.isCfgNode
      .map { node =>
        if (!dfstSet.contains(node)) {
          Some(node)
        } else {
          None
        }
      }
      .filter(_.isDefined)
      .map(_.get)
  }

  /*def getCorrespondingCall(identifier: Identifier): nodes.Call = {
    assert(identifier._astIn.asScala.toList.length == 1,
           s"$identifier has not exactly one AST parent")
    identifier._astIn.next() match {
      case parent: nodes.Call =>
        assert(parent._astIn.asScala.toList.length == 1,
               s"$parent has not exactly one AST parent")
        parent
      case weird =>
        val parentMethod = identifier.getParentMethod.get
        val lineNumber: Int =
          identifier.lineNumber.getOrElse(Integer.getInteger("-1")).toInt
        throw ReportableError(
          parentMethod.filename,
          lineNumber,
          parentMethod.fullName,
          identifier.code,
          s"$identifier has a non-call parent $weird"
        )
    }
  }*/
}
