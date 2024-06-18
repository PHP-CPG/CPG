package io.joern.bytecode.passes

import io.joern.bytecode.parser
import io.joern.bytecode.parser.PHPVersions
import io.joern.bytecode.util.unittesting.CpgFromCodeTestFixture
import io.shiftleft.semanticcpg.passes.controlflow.cfgcreation.Cfg.{AlwaysEdge, FalseEdge, TrueEdge}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CorrectDDGReassignmentTest extends AnyWordSpec with Matchers with PHPVersions  {

  for(v <- getPhpVersions) {
    implicit val version: parser.PHPVersion.Value = v

    s"cpg for PHP $version" should {
      "have no DDG edge leading from the first to the third command" in new CpgFromCodeTestFixture(
        """
          |$x = 42;
          |$x += 33;
          |echo $x;
          |""".stripMargin) {
        implicit val method: String = "dlr_main"
        ddgSuccOf("ASSIGN CV($x) int(42)") shouldBe expectedDdg(
          ("ASSIGN_OP (ADD) CV($x) int(33)", "x"))
        ddgSuccOf("ASSIGN_OP (ADD) CV($x) int(33)") shouldBe expectedDdg(
          ("ECHO CV($x)", "x"))
      }

      "have no DDG edge leading before the if" in new CpgFromCodeTestFixture(
        """
          |$x += 42;
          |if($x == 33) {
          |  $x = 11;
          |} else {
          |  $x = 33;
          |}
          |echo $x;
          |""".stripMargin) {
        implicit val method: String = "dlr_main"
        ddgSuccOf("ASSIGN_OP (ADD) CV($x) int(42)") shouldBe expectedDdg(
          ("IS_EQUAL CV($x) int(33)", "x"))
        cfgSuccOf("ASSIGN_OP (ADD) CV($x) int(42)") shouldBe expectedCfg(
          ("CV($x)", AlwaysEdge))
        ddgSuccOf("IS_EQUAL CV($x) int(33)") shouldBe Set()
        cfgSuccOf("IS_EQUAL CV($x) int(33)") shouldBe expectedCfg(
          ("T2", AlwaysEdge))
        cfgSuccOf("T2 = IS_EQUAL CV($x) int(33)", "T2") shouldBe expectedCfg(
          ("T2 = IS_EQUAL CV($x) int(33)", AlwaysEdge))
        ddgSuccOf("T2 = IS_EQUAL CV($x) int(33)") shouldBe expectedDdg(
          ("JMPZ T2 int(5)", "T2"))
        cfgSuccOf("T2 = IS_EQUAL CV($x) int(33)") shouldBe expectedCfg(
          ("T2", AlwaysEdge))
        ddgSuccOf("JMPZ T2 int(5)") shouldBe Set()
        cfgSuccOf("JMPZ T2 int(5)") shouldBe expectedCfg(("CV($x)", TrueEdge),
          ("CV($x)", FalseEdge))
        ddgSuccOf("ASSIGN CV($x) int(11)") shouldBe expectedDdg(
          ("ECHO CV($x)", "x"))
        cfgSuccOf("ASSIGN CV($x) int(33)") shouldBe expectedCfg(
          ("CV($x)", AlwaysEdge))
      }

      "have one DDG edge leading before the if" in new CpgFromCodeTestFixture(
        """
          |$x += 42;
          |if($x == 33) {
          | $x += 11;
          |} else {
          | $y = 11;
          |}
          |echo $x;
          |""".stripMargin) {
        implicit val method: String = "dlr_main"
        ddgSuccOf("ASSIGN_OP (ADD) CV($x) int(42)") shouldBe expectedDdg(
          ("IS_EQUAL CV($x) int(33)", "x"),
          ("ASSIGN_OP (ADD) CV($x) int(11)", "x"),
          ("ECHO CV($x)", "x"))
        ddgSuccOf("ASSIGN_OP (ADD) CV($x) int(11)") shouldBe expectedDdg(
          ("ECHO CV($x)", "x"))
        ddgSuccOf("ASSIGN CV($y) int(11)") shouldBe Set()
      }

      "have one DDG edge leading before the loop and one into" in new CpgFromCodeTestFixture(
        """
          |$x += 42;
          |foreach($array as $element) {
          | $x += 33;
          |}
          |echo $x;
          |""".stripMargin) {
        implicit val method: String = "dlr_main"
        ddgSuccOf("ASSIGN_OP (ADD) CV($x) int(42)") shouldBe expectedDdg(
          ("ASSIGN_OP (ADD) CV($x) int(33)", "x"),
          ("ECHO CV($x)", "x"))
        ddgSuccOf("ASSIGN_OP (ADD) CV($x) int(33)") shouldBe expectedDdg(
          ("ASSIGN_OP (ADD) CV($x) int(33)", "x"),
          ("ECHO CV($x)", "x"))
      }

      "generate a proper string for a nesting loops where the outer one is productive" in new CpgFromCodeTestFixture(
        """
          |$x = "start";
          |foreach($t as $a) {
          |   $x += "outerloop";
          |   foreach($p as $g) {
          |     $x += "innerloop";
          |   }
          |}
          |echo $x;
          |""".stripMargin
      ) {
        implicit val method: String = "dlr_main"
        ddgSuccOf("ASSIGN CV($x) string(\"start\")") shouldBe
          expectedDdg(("ASSIGN_OP (ADD) CV($x) string(\"outerloop\")", "x"),
            ("ECHO CV($x)", "x"))
        ddgSuccOf("ASSIGN_OP (ADD) CV($x) string(\"outerloop\")") shouldBe
          expectedDdg(("ASSIGN_OP (ADD) CV($x) string(\"innerloop\")", "x"),
            ("ASSIGN_OP (ADD) CV($x) string(\"outerloop\")", "x"),
            ("ECHO CV($x)", "x"))
        ddgPredOf("ASSIGN_OP (ADD) CV($x) string(\"innerloop\")") shouldBe
          expectedDdg(("ASSIGN_OP (ADD) CV($x) string(\"outerloop\")", "x"),
            ("ASSIGN_OP (ADD) CV($x) string(\"innerloop\")", "x"))
      }
    }
  }
}
