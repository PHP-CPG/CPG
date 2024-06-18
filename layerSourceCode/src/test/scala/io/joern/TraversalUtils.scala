package io.joern

import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import overflowdb.{Edge, Node}

import scala.util.control.Breaks.break

trait TraversalUtils extends Matchers {
  val fixture: AbstractCpgTestFixture

  def getMethods(name: String): List[Node] = {
    fixture.cpg.method.nameExact(name).l
  }

  def getSingleFile(name: String): Node = {
    fixture.cpg.file.nameExact(name).l match {
      case List(x) => x
      case _       => fail
    }
  }

  def followEdge(node: Node,
                 edge: String,
                 direction: String,
                 labels: List[(String, AnyRef)] = List()): List[Node] = {
    var collection: List[Edge] = List()

    val iterator = direction match {
      case "OUT"  => node.outE(edge)
      case "IN"   => node.inE(edge)
      case "BOTH" => node.bothE(edge)
    }

    while (iterator.hasNext) {
      val next = iterator.next
      var fullMatch = true
      for ((key, value) <- labels) {
        if (next.property(key) != value) {
          fullMatch = false
          break
        }
      }
      if (fullMatch) {
        collection = next :: collection
      }
    }
    direction match {
      case "OUT" => collection.map(edge => edge.inNode())
      case "IN"  => collection.map(edge => edge.outNode())
    }

  }

  def out(node: Node,
          edge: String,
          labels: List[(String, AnyRef)] = List()): List[Node] = {
    followEdge(node, edge, "OUT", labels)
  }

  def in(node: Node,
         edge: String,
         labels: List[(String, AnyRef)] = List()): List[Node] = {
    followEdge(node, edge, "IN", labels)
  }
}
