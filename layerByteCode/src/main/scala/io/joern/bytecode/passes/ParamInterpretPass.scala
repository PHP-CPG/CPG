package io.joern.bytecode.passes

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.{CpgPass, DiffGraph}
import io.shiftleft.semanticcpg.language._

class ParamInterpretPass(cpg: Cpg) extends CpgPass(cpg) {
  override def run(): Iterator[DiffGraph] = {
    val diffGraph = DiffGraph.newBuilder
    cpg.method.l.foreach { method =>
      val params = method.parameter.sortBy(_.order)
      val recvCalls = method.call.name("RECV(_INIT)?").l

      params.filter(_.name.forall(Character.isDigit)).foreach { param =>
        val index = param.name.toInt - 1
        val name = recvCalls
          .lift(index)
          .toList
          .flatten
          .inCall
          .argument
          .order(0)
          .code
          .headOption

        diffGraph.addNodeProperty(param, "NAME", name.getOrElse(""))
      }
    }
    Iterator(diffGraph.build())
  }
}
