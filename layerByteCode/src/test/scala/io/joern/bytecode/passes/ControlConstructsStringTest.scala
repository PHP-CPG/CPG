package io.joern.bytecode.passes

import io.joern.bytecode.parser
import io.joern.bytecode.parser.PHPVersions
import io.joern.bytecode.util.unittesting.{AbstractCpgTestFixture, CpgFromCodeTestFixture}
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.passes.controlflow.cfgcreation.Cfg.AlwaysEdge
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.jdk.CollectionConverters._

class ControlConstructsStringTest extends AnyWordSpec with Matchers with PHPVersions  {

  for(v <- getPhpVersions) {
    implicit val version: parser.PHPVersion.Value = v

    val fixture: AbstractCpgTestFixture = CpgFromCodeTestFixture(
      """switch($test) {
        | case "to":
        |     echo("first");
        |     break;
        | case "cc":
        |     echo("second");
        |     break;
        | case "bcc":
        |     echo("third");
        |     break;
        |}
        |echo("done");
        |""".stripMargin
    )

    s"resulting cpg for PHP $version" should {
      "have correct cfg" in {
        implicit val method: String = "dlr_main"
        val main = fixture.cpg.method.l.head
        main.code shouldBe "dlr_main()"
        //println(main.ast.isCfgNode.map(_.code).l)
        val switchParent =
          """SWITCH_STRING CV($test) "to": 8, "cc": 10, "bcc": 12, "default": 14"""
        fixture.cfgSuccOf("dlr_main()") shouldBe fixture.expectedCfg(
          ("METHOD BLOCK", AlwaysEdge))
        fixture.cfgSuccOf("METHOD BLOCK") shouldBe fixture.expectedCfg(
          switchParent,
          ("CV($test)", AlwaysEdge))
        fixture.cfgSuccOf(switchParent, "CV($test)") shouldBe fixture.expectedCfg(
          switchParent,
          ("to", AlwaysEdge))
        fixture.cfgSuccOf(switchParent, "to") shouldBe fixture.expectedCfg(
          switchParent,
          ("8", AlwaysEdge))
        fixture.cfgSuccOf(switchParent, "8") shouldBe fixture.expectedCfg(
          switchParent,
          ("cc", AlwaysEdge))
        fixture.cfgSuccOf(switchParent, "cc") shouldBe fixture.expectedCfg(
          switchParent,
          ("10", AlwaysEdge))
        fixture.cfgSuccOf(switchParent, "10") shouldBe fixture.expectedCfg(
          switchParent,
          ("bcc", AlwaysEdge))
        fixture.cfgSuccOf(switchParent, "bcc") shouldBe fixture.expectedCfg(
          switchParent,
          ("12", AlwaysEdge))
        fixture.cfgSuccOf(switchParent, "12") shouldBe fixture.expectedCfg(
          switchParent,
          ("default", AlwaysEdge))
        fixture.cfgSuccOf(switchParent, "default") shouldBe fixture.expectedCfg(
          switchParent,
          ("14", AlwaysEdge))
        fixture.cfgSuccOf(switchParent, "14") shouldBe fixture.expectedCfg(
          (switchParent, AlwaysEdge))
        fixture.cpg.call
          .codeExact(switchParent)
          .head
          ._cfgOut
          .asScala
          .toList
          .length shouldBe 5
      }
    }
  }
}
