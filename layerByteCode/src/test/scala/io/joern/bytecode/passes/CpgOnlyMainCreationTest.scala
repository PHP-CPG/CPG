package io.joern.bytecode.passes

import io.joern.bytecode.parser.{PHPVersion, PHPVersions}
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes, nodes}
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.passes.controlflow.cfgcreation.Cfg.AlwaysEdge
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import overflowdb.Node

import java.io.File
import scala.jdk.CollectionConverters._

class CpgOnlyMainCreationTest
    extends AnyWordSpec
    with Matchers with PHPVersions {

  for(v <- getPhpVersions) {
    implicit val version: PHPVersion.Value = v

    val fixture: CpgTestFixture = CpgTestFixture("onlyMainCreation")

    def getMethods(name: String): List[Node] = {
      fixture.cpg.method.nameExact(name).l
    }

    def getSingleFile(name: String): Node = {
      fixture.cpg.file.nameExact(name).l match {
        case List(x) => x
        case _ => fail()
      }
    }


    s"CFG layout for PHP $version" should {
      "have a single file" in {
        getSingleFile(fixture.files.head.getPath)
      }
    }

    s"CFG Operations Graphs for PHP $version" should {
      "have a $_main method with a single block" in {
        val main: List[Node] = getMethods("dlr_main")
        main.length shouldBe 1
        val mainChildren = main.flatMap(
          _.out(EdgeTypes.AST).asScala
            .filter(_.label == NodeTypes.BLOCK))
        mainChildren.length shouldBe 1
      }
      "have a main block with 6 instructions" in {
        val main: List[Node] = getMethods("dlr_main")
        val instructions = main.flatMap(
          _.out(EdgeTypes.AST).asScala
            .filter(_.label == NodeTypes.BLOCK)
            .flatMap(_.out(EdgeTypes.AST).asScala))
        assert(instructions.length == 6)
      }
    }

    s"CPG CFG for PHP $version" should {
      "have a CFG edge coming from $_main" in {
        implicit val method: String = "dlr_main"
        fixture.cfgSuccOf("dlr_main()") shouldBe fixture.expectedCfg(
          ("METHOD BLOCK", AlwaysEdge))
        fixture.cfgSuccOf("METHOD BLOCK") shouldBe fixture.expectedCfg(
          ("1", AlwaysEdge))
      }
      "have a CFG edge leading into the $_main METHOD_RETURN node" in {
        val main = getMethods("dlr_main")
        main.length shouldBe 1
        val methodReturn = main.head.out(EdgeTypes.AST).asScala.toList.filter(vertex =>
          vertex.label == NodeTypes.METHOD_RETURN)
        methodReturn.length shouldBe 1
        val preFlow = methodReturn.head.in(EdgeTypes.CFG).asScala.toList
        preFlow.length shouldBe 1
        preFlow.head.property("CODE") shouldBe "RETURN int(1)"
      }
      "have 16 CFG steps in between the start and end of the method body" in {
        val main: List[nodes.Method] = fixture.cpg.method("dlr_main").l
        main.length shouldBe 1
        var next: nodes.CfgNode = main.head
          ._cfgOut
          .asScala
          .map(_.asInstanceOf[nodes.CfgNode])
          .toList
          .head
        for (_ <- Range(0, 20)) {
          next =
            next._cfgOut.asScala.map(_.asInstanceOf[nodes.CfgNode]).toList.head
        }
        next.code shouldBe "RETURN int(1)"
      }
    }
  }
}
