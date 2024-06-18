package io.joern.php.passes

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool, ParallelCpgPass}

class MetaDataPass(cpg: Cpg, keyPool: IntervalKeyPool)
    extends ParallelCpgPass[String](
      cpg,
      keyPools = Some(keyPool.split(1))
    ) {
  // don't care at the moment
  override def partIterator: Iterator[String] = List("").iterator

  override def runOnPart(part: String): Iterator[DiffGraph] = {
    implicit val diffGraph: DiffGraph.Builder = DiffGraph.newBuilder
    val metaDataNode = nodes.NewMetaData().language("PHP").version("0.1")
    diffGraph.addNode(metaDataNode)
    Iterator(diffGraph.build())
  }
}
