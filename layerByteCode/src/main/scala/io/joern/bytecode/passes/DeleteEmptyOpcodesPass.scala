package io.joern.bytecode.passes

import io.joern.reporting.Reporting
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, nodes}
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool, ParallelCpgPass}
import io.shiftleft.semanticcpg.language._

import scala.jdk.CollectionConverters._

class DeleteEmptyOpcodesPass(methods: Seq[nodes.Method],
                             cpg: Cpg,
                             keyPool: IntervalKeyPool)
    extends ParallelCpgPass[nodes.Method](cpg: Cpg,
                                          keyPools =
                                            Some(keyPool.split(methods.length)))
    with Reporting {

  private val emptyOpcodeNames = List("EXT_NOP", "EXT_STMT", "NOP")
  override val name = "DeleteEmptyOpcodePass"
  override def partIterator: scala.Iterator[nodes.Method] = methods.iterator

  var somethingChanged: Boolean = false

  //todo: this is a horrible way of deleting empty opcodes as this logic needs to be called n times for n being
  //      the max amount of opcodes over the set of all methods
  override def runOnPart(part: Method): Iterator[DiffGraph] = {
    implicit val diffGraph: DiffGraph.Builder = DiffGraph.newBuilder
    withErrorReporting(part) {
      emptyOpcodeNames.find(elem => part.astMinusRoot.isCall(elem).hasNext) match {
        case Some(emptyCallString) =>
          val emptyOpcode = part.astMinusRoot.isCall(emptyCallString).next()
          val previousNodes = emptyOpcode
            .in(EdgeTypes.CFG)
            .asScala
            .toList
            .map(_.asInstanceOf[nodes.CfgNode])
          val nextNodes = emptyOpcode
            .out(EdgeTypes.CFG)
            .asScala
            .toList
            .map(_.asInstanceOf[nodes.CfgNode])
          for (previous <- previousNodes) {
            for (next <- nextNodes) {
              diffGraph.addEdge(previous, next, EdgeTypes.CFG)
            }
          }
          diffGraph.removeNode(emptyOpcode)
          this.synchronized {
            somethingChanged = true
          }
        case None => None
      }
    }
    Iterator(diffGraph.build())
  }
}

object DeleteEmptyOpcodesPass {

  def getMethodDeclarations(cpg: Cpg): Seq[nodes.Method] = {
    cpg.method.l
  }

}
