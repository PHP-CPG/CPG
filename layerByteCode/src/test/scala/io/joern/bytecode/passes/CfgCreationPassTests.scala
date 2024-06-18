package io.joern.bytecode.passes

import io.joern.bytecode.parser
import io.joern.bytecode.parser.PHPVersions
import io.joern.bytecode.util.unittesting.CpgFromCodeTestFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method}
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.passes.controlflow.cfgcreation.Cfg.AlwaysEdge
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import overflowdb.traversal.NodeOps

class CfgCreationPassTests extends AnyWordSpec with Matchers with PHPVersions {

  for(v <- getPhpVersions) {
    implicit val version: parser.PHPVersion.Value = v

    s"Cfg for PHP $version" should {

      "allow retrieving arguments" in new CpgFromCodeTestFixture(
        "$x = 2;") {
        val call: Call = cpg.call.codeExact("ASSIGN CV($x) int(2)").head
        call.start.argument.size shouldBe 2
      }

      // Empty functions are removed entirely, so, in contrast to the tests in the C frontend,
      // we do not test here for the correct translation of empty functions into entry + exit node

      "be correct for assignment" in new CpgFromCodeTestFixture(
        "$x = 2;") {
        implicit val method: String = "dlr_main"
        val main: Method = this.cpg.method.l.head
        main.code shouldBe "dlr_main()"
        cfgSuccOf("dlr_main()") shouldBe expectedCfg(("METHOD BLOCK", AlwaysEdge))
        cfgSuccOf("METHOD BLOCK") shouldBe expectedCfg(("CV($x)", AlwaysEdge))
        cfgSuccOf("CV($x)") shouldBe expectedCfg(("2", AlwaysEdge))
        cfgSuccOf("2") shouldBe expectedCfg(("ASSIGN CV($x) int(2)", AlwaysEdge))
        cfgSuccOf("ASSIGN CV($x) int(2)") shouldBe expectedCfg(("1", AlwaysEdge))
        cfgSuccOf("1") shouldBe expectedCfg(("RETURN int(1)", AlwaysEdge))
      }

      "be correct for `if` statement" in new CpgFromCodeTestFixture(
        """
          |if ($x > 10) {
          |   return 1;
          |}
          |return 0;
          |""".stripMargin) {
        implicit val method: String = "dlr_main"
        cfgSuccOf("dlr_main()") shouldBe expectedCfg(("METHOD BLOCK", AlwaysEdge))
        cfgSuccOf("METHOD BLOCK") shouldBe expectedCfg(("10", AlwaysEdge))
        cfgSuccOf("10") shouldBe expectedCfg(("CV($x)", AlwaysEdge))
        cfgSuccOf("CV($x)") shouldBe expectedCfg(
          ("IS_SMALLER int(10) CV($x)", AlwaysEdge))
      }

      "be correct for `while` loop" in new CpgFromCodeTestFixture(
        """
          |$x = 0;
          |while($x < 10) {
          |  foo($x);
          |}
          |""".stripMargin) {
        implicit val method: String = "dlr_main"
        cfgSuccOf("ASSIGN CV($x) int(0)") shouldBe expectedCfg(("5", AlwaysEdge))
        cfgSuccOf("5") shouldBe expectedCfg(("JMP int(5)", AlwaysEdge))
        cfgSuccOf("JMP int(5)") shouldBe expectedCfg(("CV($x)", AlwaysEdge))
        cfgSuccOf("CV($x)") shouldBe expectedCfg(("10", AlwaysEdge))
        cfgSuccOf("10") shouldBe expectedCfg(
          ("IS_SMALLER CV($x) int(10)", AlwaysEdge))
        cfgSuccOf("IS_SMALLER CV($x) int(10)") shouldBe expectedCfg(
          ("T3", AlwaysEdge))
        cfgSuccOf("T3 = IS_SMALLER CV($x) int(10)", "T3") shouldBe
          expectedCfg(("T3 = IS_SMALLER CV($x) int(10)", AlwaysEdge))
        cfgSuccOf("DO_FCALL_BY_NAME") shouldBe expectedCfg(("CV($x)", AlwaysEdge))
      }

      "create the correct CFG for drupal example" in new CpgFromCodeTestFixture(
        """
          |class Klasse {
          | public function Send() {
          |    try {
          |      switch($this->Mailer) {
          |        default:
          |          return $this->MailSend($header, $body);
          |      }
          |    } catch (phpmailerException $e) {
          |      echo $e->getMessage()."\n";
          |    }
          |  }
          |}
          |""".stripMargin
      ) {
        implicit val method: String = "klasse::send"
        cfgSuccOf("INIT_METHOD_CALL 0 CV($e) string(\"getmessage\")") shouldBe expectedCfg(
          ("DO_FCALL", AlwaysEdge))
      }
      "create a CFG edge after the JMP on true in a normal function" in new CpgFromCodeTestFixture(
        """function test() {
          |   if(other()) {
          |     if(ttest())
          |       echo "test";
          |   }
          |   do {
          |     echo "ttest";
          |   } while(true);
          |}
          |""".stripMargin
      ) {
        implicit val method: String = "test"
        cfgSuccOf("JMPNZ bool(true) int(7)") shouldBe expectedCfg(("ttest", AlwaysEdge))
      }
      "create a CFG edge after the JMP on true even in a class" in new CpgFromCodeTestFixture(
        """class testClass {
          |function test() {
          |   if(other()) {
          |     if(ttest())
          |       echo "test";
          |   }
          |   do {
          |     echo "ttest";
          |   } while(true);
          |}
          |}
          |""".stripMargin
      ) {
        implicit val method: String = "testclass::test"
        cfgSuccOf("JMPNZ bool(true) int(7)") shouldBe expectedCfg(("ttest", AlwaysEdge))
      }
    }
  }
}
