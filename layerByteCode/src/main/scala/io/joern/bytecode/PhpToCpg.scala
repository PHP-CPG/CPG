package io.joern.bytecode

import io.joern.bytecode.layers.PhpScpg
import io.joern.bytecode.parser.PHPVersion
import io.joern.bytecode.parser.PHPVersion.PHPVersion
import io.joern.bytecode.parser.php7.FileParser7
import io.joern.bytecode.parser.php8.FileParser8
import io.joern.bytecode.passes._
import io.joern.bytecode.passes.dataDependencyPasses.{
  AddArrayElementPass,
  DataDependencyPass
}
import io.joern.config.CPGConfig
import io.joern.reporting.{Report, Reporting}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.KeyPoolCreator
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import io.shiftleft.x2cpg.X2Cpg

import java.io.File
import scala.sys.process._

class PhpToCpg()(implicit val version: PHPVersion) extends Reporting {

  val report: Report = Report()
  override val name = "PHP2Cpg"

  private val PHP7_EXPECTED_VSTRING = "PHP 7.4.27-dev"
  private val PHP8_EXPECTED_VSTRING = "PHP 8.2.2-dev"

  def createCpg(files: List[File],
                outputPath: String,
                config: CPGConfig): Cpg = {
    val cpg = X2Cpg.newEmptyCpg(Some(outputPath))
    populateCpg(files, cpg, config)
  }

  def populateCpg(files: List[File], cpg: Cpg, config: CPGConfig): Cpg = {
    if (config.verbose) warn("verbose output enabled!")
    val keyPools = KeyPoolCreator.obtain(19).iterator

    val (fileParser, interpreter) = {
      version match {
        case io.joern.bytecode.parser.PHPVersion.V7 =>
          (new FileParser7(files, config.php7.interpreter, config.php7.phpini),
           config.php7.interpreter)
        case io.joern.bytecode.parser.PHPVersion.V8 =>
          (new FileParser8(files,
                           config.php8.interpreter,
                           config.php8.phpini,
                           config.strictParsing),
           config.php8.interpreter)
      }
    }
    // check if interpreter exists
    try {
      s"$interpreter --version".!(ProcessLogger(_ => (), err => println(err)))
    } catch {
      case _: Exception =>
        println(
          s"Couldn't run `$interpreter --version`. Did you configure the interpreter path correctly?")
        sys.exit(1)
    }

    // check if it is the right version
    val version_check_output = s"${version match {
      case PHPVersion.V7 => config.php7.interpreter
      case PHPVersion.V8 => config.php8.interpreter
    }} --version".!!
    val is_expected_version = version match {
      case PHPVersion.V7 =>
        version_check_output.startsWith(PHP7_EXPECTED_VSTRING)
      case PHPVersion.V8 =>
        version_check_output.startsWith(PHP8_EXPECTED_VSTRING)
    }
    if (!is_expected_version) {
      error("Unexpected PHP version detected!")
      version match {
        case PHPVersion.V7 =>
          error(
            "expected " + PHP7_EXPECTED_VSTRING + " but got: " + version_check_output
              .slice(0, 10))
        case PHPVersion.V8 =>
          error(
            "expected " + PHP8_EXPECTED_VSTRING + " but got " + version_check_output
              .slice(0, 10))
      }
      throw new RuntimeException("Unexpected PHP version")
    }

    val filesMethodDefinitionPairs = fileParser.run()
    report.addReport(fileParser.getReport)
    if (config.verbose) info("FileParser done.")

    if (config.passes.contains("metadata")) {
      val mdp = new MetaDataPass(cpg, keyPools.next())
      mdp.createAndApply()
      report.addReport(mdp.getReport)
    } else {
      warn("MetaDataPass has been deactivated")
    }

    // creating the basic source code AST, in method packages
    if (config.passes.contains("methodscreation")) {
      val mcp =
        new MethodsCreationPass(filesMethodDefinitionPairs,
                                cpg,
                                keyPools.next())
      mcp.createAndApply()
      report.addReport(mcp.getReport)
      if (config.verbose) info("Method creation pass done.")
    } else {
      warn("MethodsCreationPass has been deactivated")
    }

    // create the newly introduced different types
    if (config.passes.contains("typecreation")) {
      val tcp =
        new TypeCreationPass(TypeCreationPass.getTypeIndicatingNodes(cpg), cpg)
      tcp.createAndApply()
      report.addReport(tcp.getReport)
    } else {
      warn("TypeCreationPass has been deactivated")
    }

    if (config.passes.contains("cfgintrabbcreation")) {
      val cintrabcp = new CfgIntraBBCreationPass(filesMethodDefinitionPairs,
                                                 cpg,
                                                 keyPools.next())
      cintrabcp.createAndApply()
      report.addReport(cintrabcp.getReport)
    } else {
      warn("CfgIntraBBCreationPass has been deactivated")
    }

    if (config.passes.contains("cfginterbbcreation")) {
      val cinterbcp = new CfgInterBBCreationPass(filesMethodDefinitionPairs,
                                                 cpg,
                                                 keyPools.next())
      cinterbcp.createAndApply()
      report.addReport(cinterbcp.getReport)
    } else {
      warn("CfgInterBBCreationPass has been deactivated")
    }
    if (config.verbose && config.passes.contains("cfgintrabbcreation"))
      info("CFG creation done.")

    if (config.passes.contains("deleteunreachablecode")) {
      val ducp =
        new DeleteUnreachableCodePass(cpg.method.l, cpg, keyPools.next())
      ducp.createAndApply()
      report.addReport(ducp.getReport)
      if (config.verbose) info("DCE done.")
    } else {
      warn("DeleteUnreachableCodePass has been deactivated")
    }

    if (config.passes.contains("dominator")) {
      val dp = new DominatorPass(cpg.method.l, cpg, keyPools.next())
      dp.createAndApply()
      report.addReport(dp.getReport)
    } else {
      warn("DominatorPass has been deactivated")
    }

    if (config.passes.contains("postdominator")) {
      val dpp = new DominatorPass(cpg.method.l, cpg, keyPools.next(), true)
      dpp.createAndApply()
      report.addReport(dpp.getReport)
    } else {
      warn("PostDominatorPass has been deactivated")
    }

    if (config.verbose && config.passes.contains("dominator") || config.passes
          .contains("postdominator")) info("Dominator(s) done.")

    if (config.passes.contains("datadependency")) {
      val ddp = new DataDependencyPass(cpg.method.l, cpg, keyPools.next())
      ddp.createAndApply()
      report.addReport(ddp.getReport)
      info("DDG done.")
      val aaep = new AddArrayElementPass(cpg.method.l, cpg, keyPools.next())
      aaep.createAndApply()
      report.addReport(aaep.getReport)
      info("DDG-AAE done.")
    } else {
      warn("DataDependencyPass has been deactivated")
    }

    // this is basically a fixpoint iteration until all empty opcodes are deleted
    // horribly bad in terms of performance and style ... but it works .. hopefully
    // @Fabian: Is this smart? I am not really sure what the keypool does
    if (config.passes.contains("deleteemptyopcodes")) {
      val deleteKeyPool = keyPools.next()
      var somethingChanged = false
      do {
        val deop = new DeleteEmptyOpcodesPass(
          DeleteEmptyOpcodesPass.getMethodDeclarations(cpg),
          cpg,
          deleteKeyPool)
        deop.createAndApply()
        somethingChanged = deop.somethingChanged
        report.addReport(deop.getReport)
      } while (somethingChanged)
      if (config.verbose) info("Empty opcode deletion done.")
    } else {
      warn("DeleteEmptyOpcodesPass has been deactivated")
    }

    if (config.passes.contains("deleteemptymethods")) {
      val demd = new DeleteEmptyMethodDeclarations(
        DeleteEmptyMethodDeclarations.getMethodDeclarations(cpg),
        cpg,
        keyPools.next())
      demd.createAndApply()
    } else {
      warn("DeleteEmptyMethodsPass has been deactivated")
    }

    if (config.passes.contains("inheritance")) {
      val ip = new InheritancePass(
        InheritancePass.getInheritanceIndicatingCalls(cpg),
        cpg,
        keyPools.next())
      ip.createAndApply()
      report.addReport(ip.getReport)
    } else {
      warn("InheritancePass has been deactivated")
    }

    // we first need to create possible stub methods
    if (config.passes.contains("createstubmethods")) {
      val stub =
        new CreateStubMethodNodesPass(cpg,
                                      keyPools.next(),
                                      strict = config.strictLinking,
                                      interpreter)
      stub.createAndApply()
      report.addReport(stub.getReport)
      report.addReport(stub.getLinkingReport)
      info("Stub creation done.")
    } else {
      warn("CreateStubMethodsPass has been deactivated")
    }

    // for call linking to work properly
    if (config.passes.contains("callfinishing")) {
      val call =
        new CallFinishingPass(cpg.method.l,
                              cpg,
                              keyPools.next(),
                              strict = config.strictLinking)
      call.createAndApply()
      report.addReport(call.getReport)
      info("Call finishing pass done.")
    } else {
      warn("CallFinishingPass has been deactivated")
    }

    if (config.passes.contains("namespacemembercreation")) {
      val nmcp = new NamespaceMemberCreationPass(
        NamespaceMemberCreationPass.getNamespaceMemberRelevantFunctions(cpg),
        cpg,
        keyPools.next())
      nmcp.createAndApply()
      report.addReport(nmcp.getReport)
    } else {
      warn("NamespaceMemberCreationPass has been deactivated")
    }

    if (config.passes.contains("localidentification")) {
      val lip = new LocalIdentificationPass(
        LocalIdentificationPass.getRelevantMethodDeclarations(cpg),
        cpg,
        keyPools.next())
      lip.createAndApply()
      report.addReport(lip.getReport)
    } else {
      warn("LocalIdentificationPass has been deactivated")
    }

    // Enhancements:
    val context = new LayerCreatorContext(cpg)
    val phpscpg = new PhpScpg()
    phpscpg.run(context)
    report.addReport(phpscpg.getReport)
    if (config.verbose) info("Finished CPG population.")
    cpg
  }

  def getFinalReport: Report = {
    report.addReport(this.getReport)
    report
  }

}
