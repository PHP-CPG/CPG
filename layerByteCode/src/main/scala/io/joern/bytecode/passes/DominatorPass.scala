/**
  * This algorithm is based on Compiler Bau II by Alfred V. Aho, Ravi Sethis, and Jeffrey D. Ullmann
  * Chapter 10.9 Algorithm 10.16
  */
package io.joern.bytecode.passes

import io.joern.bytecode.util.extensions.NodeExtension._
import io.joern.reporting.{ReportableError, Reporting}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{
  CfgNode,
  Method,
  MethodReturn
}
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, nodes}
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool, ParallelCpgPass}
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.jIteratortoTraversal

import scala.collection.mutable.{Map => MMap, Set => MSet}
import scala.collection.{Iterator, Seq}

class DominatorPass(methods: Seq[nodes.Method],
                    cpg: Cpg,
                    keyPool: IntervalKeyPool,
                    postDomination: Boolean = false)
    extends ParallelCpgPass[nodes.Method](cpg: Cpg,
                                          keyPools =
                                            Some(keyPool.split(methods.size)))
    with Reporting {

  override val name: String =
    if (!postDomination) "DominatorPass" else "PostDominatorPass"

  override def partIterator: Iterator[Method] = methods.iterator

  //private val debugPass = "PostDominatorPass"
  //private val debugType = Method.Label

  override def runOnPart(part: Method): Iterator[DiffGraph] = {
    //if(name == debugPass) {
    //  println(s"######## $name + ${part.name} #########")
    //  println()
    //}
    val diffGraph = DiffGraph.newBuilder
    withErrorReporting(part) {
      if (!postDomination) {
        calculateDominatorTree(part, diffGraph)
      } else {
        val method_return = part.ast.filter(_.isInstanceOf[MethodReturn]).l
        calculateDominatorTree(method_return.head.asInstanceOf[CfgNode],
                               diffGraph)
      }
    }
    Iterator(diffGraph.build())
  }

  /**
    *
    * only stuff belonging to the coherent CFG starting from start belong to the domination
    * calculation
    *
    * @param start the cfg start
    * @return the map (and implicitly also the order/involved nodes)
    */
  private def initiateDominatorMap(
      start: CfgNode): MMap[CfgNode, Set[CfgNode]] = {
    var frontier: Set[CfgNode] = Set(start)
    val collection: MSet[CfgNode] = MSet()
    while (frontier.nonEmpty) {
      val newFrontier: MSet[CfgNode] = MSet()
      frontier.foreach { node =>
        // get all connected nodes incoming and add them to the new frontier
        newFrontier.addAll(
          node
            .out(EdgeTypes.CFG)
            .cast[CfgNode]
        )
        // get all connected nodes outgoing and add them to the new frontier
        newFrontier.addAll(
          node
            .in(EdgeTypes.CFG)
            .cast[CfgNode]
        )
        // add the current node to the collection as we handled it
        collection.addOne(node)
      }
      // make the frontier the new frontier minus the already collected nodes so no double visiting
      frontier = newFrontier.toSet -- collection.toSet
    }
    val ret: MMap[CfgNode, Set[CfgNode]] = MMap()
    collection.foreach { node =>
      ret.addOne(node -> collection.toSet)
    }
    ret.addOne(start -> Set(start))
    ret
  }

  /*private def initiateDominatorMap(
      start: CfgNode): MMap[CfgNode, Set[CfgNode]] = {
    val methodNode = start.getParentMethod.getOrElse(
      throw new RuntimeException(
        s"given start node $start has no parent method"))
    val dominationMap = MMap[CfgNode, Set[CfgNode]]()
    // for the start node only the start node is dominator
    val allNodesList: Seq[CfgNode] = methodNode.ast.isCfgNode.l ++ List(start)
    methodNode.ast.isCfgNode.foreach { node =>
      // creating new instance
      val allNodes: Set[CfgNode] = Set.from(allNodesList)
      // initially all nodes are dominators
      dominationMap.addOne((node, allNodes))
    }
    dominationMap.addOne((start, Set(start)))
    dominationMap
  }*/

  /*private def calculateOrder(part: CfgNode): List[CfgNode] = {
    // order is only relevant for optimization and the below will be trivially work
    // though not necessarily fast O(n^2) whereas it is possible to achieve O(n)
    val methodNode = part.getParentMethod.getOrElse(
      throw new ReportableError("na",-1,"na","na",
        s"given start node $start has no parent method"))
    methodNode.ast.isCfgNode.l
  }*/

  private def calculateDominationSet(
      node: CfgNode,
      dominationMap: MMap[CfgNode, Set[CfgNode]]): Set[CfgNode] = {
    val next: Seq[Set[CfgNode]] =
      name match {
        case "DominatorPass" =>
          node
          // pred nodes of the given node
            .in(EdgeTypes.CFG)
            .cast[CfgNode]
            .toList
            .map(dominationMap)
        case "PostDominatorPass" =>
          node
          // post nodes of the node
            .out(EdgeTypes.CFG)
            .cast[CfgNode]
            .toList
            .map(dominationMap)
      }

    //if(node.label == debugType && name == debugPass) {
    //  println(s"prev (${node.code}/${node.id()}):" + next.map(_.map(_.code)))
    //}

    val intersection: Set[CfgNode] = if (next.nonEmpty) {
      next.reduce((lhs, rhs) => lhs.intersect(rhs))
    } else {
      Set()
    }
    intersection ++ Set(node)
  }

  private def calculateDominatorTree(start: CfgNode,
                                     diffGraph: DiffGraph.Builder): Unit = {
    val dominationMap: MMap[CfgNode, Set[CfgNode]] = initiateDominatorMap(start)
    /*name match {
        case "DominatorPass" =>
          initiateDominatorMap(start)
        case "PostDominatorPass" =>
          initiateDominatorMap(
            start.getParentMethod.getOrElse(
              throw new RuntimeException("given node has no parent method")))
      }*/
    val preOrder: Seq[CfgNode] = dominationMap.keySet.toList
    if (preOrder.toSet != dominationMap.keySet) {
      val correspondingMethod = start.getParentMethod.get
      val setString = preOrder.toList
        .sortBy(_.id())
        .map { elem: CfgNode =>
          s"${elem.label}/${elem.code}"
        }
        .mkString(",")
      val mapString = dominationMap.keySet.toList
        .sortBy(_.id())
        .map(elem => s"${elem.label}/${elem.code}")
        .mkString(",")
      val msg: String = s"""the order set and the domination map are different:
           | $setString
           | $mapString""".stripMargin
      throw ReportableError(correspondingMethod, start, msg)
    }
    var changed = false
    do {
      changed = false
      preOrder.foreach { currentNode =>
        //if(currentNode.label == debugType && name == debugPass) {
        //  println()
        //  println(s"${currentNode.code}")
        //}
        val newDominationSet =
          calculateDominationSet(currentNode, dominationMap)

        //if(currentNode.label == debugType && name == debugPass) {
        //  println(s"old set ${dominationMap(currentNode).map(_.code).mkString(",")}")
        //  println(s"new set ${newDominationSet.map(_.code).mkString(",")}")
        //  println()
        //}

        if (newDominationSet != dominationMap(currentNode)) {
          dominationMap.addOne((currentNode, newDominationSet))
          changed = true
        }
      }
    } while (changed)
    dominationMap.foreach {
      case (node, dominators) =>
        dominators.foreach(dom => addDominationEdge(dom, node, diffGraph))
    }
  }

  private def addDominationEdge(start: CfgNode,
                                end: CfgNode,
                                diffGraph: DiffGraph.Builder): Unit = {
    if (start != end) {
      name match {
        case "DominatorPass" =>
          //if(start.label == debugType && name == debugPass) {
          //  println(s"${start.label} - DOMINATE-> ${end.label}")
          //}
          diffGraph.addEdge(start, end, EdgeTypes.DOMINATE)
        case "PostDominatorPass" =>
          //if (start.label == debugType && name == debugPass) {
          //  println(s"${start.label} -POST_DOMINATE-> ${end.label}/${end.code}")
          //}
          diffGraph.addEdge(start, end, EdgeTypes.POST_DOMINATE)
      }
    }
  }

}
