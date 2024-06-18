package io.joern.bytecode.passes

import io.joern.reporting.Reporting
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool, ParallelCpgPass}
import io.shiftleft.semanticcpg.language.types.structure.FileTraversal

class MetaDataPass(cpg: Cpg, keyPool: IntervalKeyPool)
    extends ParallelCpgPass[String](
      cpg,
      keyPools = Some(keyPool.split(1))
    )
    with Reporting {

  override val name = "MetaDataPass"

  // don't care at the moment
  override def partIterator: Iterator[String] = List("").iterator

  override def runOnPart(part: String): Iterator[DiffGraph] = {
    implicit val diffGraph: DiffGraph.Builder = DiffGraph.newBuilder
    val metaDataNode = nodes.NewMetaData().language("PHP").version("1.0")
    diffGraph.addNode(metaDataNode)
    diffGraph.addNode(nodes.NewFile().name(FileTraversal.UNKNOWN).order(0))
    Iterator(diffGraph.build())
  }
}
