package io.joern.bytecode.layers

import io.joern.bytecode.passes.ParamInterpretPass
import io.joern.reporting.Reporting
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.CpgPassBase
import io.shiftleft.semanticcpg.layers.{
  LayerCreator,
  LayerCreatorContext,
  LayerCreatorOptions
}
import io.shiftleft.semanticcpg.passes.base.{ContainsEdgePass, NamespaceCreator}

object PhpScpg {
  val overlayName: String = "phpsemanticcpg"
  val description: String = "linked code property graph (OSS-PHP)"

  def defaultOpts = new LayerCreatorOptions()
}

class PhpScpg(optionsUnused: LayerCreatorOptions = null)
    extends LayerCreator
    with Reporting {

  override val name = "PhpScpg"
  override val overlayName: String = PhpScpg.overlayName
  override val description: String = PhpScpg.description

  override def create(context: LayerCreatorContext,
                      serializeInverse: Boolean): Unit = {
    withErrorReporting() {
      val cpg = context.cpg
      val enhancementExecList = createEnhancementExecList(cpg)
      enhancementExecList.zipWithIndex.foreach {
        case (pass, index) =>
          val serializedCpg =
            initSerializedCpg(context.outputDir, pass.name, index)
          pass.createApplySerializeAndStore(serializedCpg, serializeInverse)
          serializedCpg.close()
      }
    }
  }

  private def createEnhancementExecList(cpg: Cpg): Iterator[CpgPassBase] = {
    Iterator(
      //new MethodDecoratorPass(cpg),
      // new CapturingLinker(cpg), // was removed in https://github.com/ShiftLeftSecurity/codepropertygraph/pull/1255
      // new Linker(cpg),
      // caller linker does not work as call start (with the name) and call end
      // are not the same and each opcode is an unlinkable call
      //new StaticCallLinker(cpg),
      //new DynamicCallLinker(cpg),
      //new MethodRefLinker(cpg),
      // new MemberAccessLinker(cpg),
      // new MethodExternalDecoratorPass(cpg), // removed in https://github.com/ShiftLeftSecurity/codepropertygraph/pull/1489/
      new ContainsEdgePass(cpg),
      new NamespaceCreator(cpg),
      // I rather implement this on my own to get a grip on the underlying algorithm
      // and to ease debugging in case something does not work (which doesn't as I am getting errors)
      // new CfgDominatorPass(cpg),
      // no idea what this is doing
      // new CdgPass(cpg),
      new ParamInterpretPass(cpg),
    )
  }

  /* 2021-26-11: probe was removed in https://github.com/ShiftLeftSecurity/codepropertygraph/pull/1488
  override def probe(cpg: Cpg): Boolean = {
    cpg.graph.nodes(NodeTypes.METHOD_PARAMETER_OUT).hasNext
  }*/
}
