package io.joern.bytecode.passes

import io.joern.bytecode.parser
import io.joern.bytecode.parser.PHPVersions
import io.joern.bytecode.util.unittesting.CpgFromCodeTestFixture
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DominatorPassTest extends AnyWordSpec with Matchers with PHPVersions {

  for(v <- getPhpVersions) {
    implicit val version: parser.PHPVersion.Value = v

    s"cpg pre dominator for PHP $version" should {
      "have trivial domination tree" in new CpgFromCodeTestFixture(
        """echo "test";""".stripMargin
      ) {
        cpg.method("dlr_main").head.dominates.l.map(_.code).toSet shouldBe
          Set("ECHO string(\"test\")", "test", "1", "RETURN int(1)", "<empty>", "METHOD BLOCK")
        val echo: Call = cpg.call.codeExact("ECHO string(\"test\")").head
        echo.dominates.l.map(_.code).toSet shouldBe
          Set("1", "RETURN int(1)", "<empty>")
      }

      "work with an if clause" in new CpgFromCodeTestFixture(
        """if($x == true) {
          |   echo "true";
          |} else {
          |   echo "false";
          |}
          |echo "finally";
          |""".stripMargin
      ) {
        val jmp: Call = cpg.call.codeExact("JMP int(5)").head
        // jmp does not dominate anything
        jmp.dominates.l.map(_.code).toSet shouldBe
          Set()
        val echoTrue: Call = cpg.call.codeExact("ECHO string(\"true\")").head
        echoTrue.dominates.l.map(_.code).toSet shouldBe
          Set("JMP int(5)", "5")
      }
      "work with function definitions" in new CpgFromCodeTestFixture(
        """function fu($x) {
          |  echo $x;
          |}
          |fu("42");
          |""".stripMargin
      ) {
        cpg.method("fu").l.head.dominates.nonEmpty shouldBe true
      }
      "work with function definition using return stmt" in new CpgFromCodeTestFixture(
        """function foo($x) {
          |   echo $x;
          |   return $x;
          |}
          |""".stripMargin
      ) {
        cpg.method.nameExact("foo").l.head.dominates.nonEmpty shouldBe true
      }
      "work with constructor" in new CpgFromCodeTestFixture(
        """class test {
          |
          | function __construct() {
          |   $this->test = 42;
          | }
          |
          |}""".stripMargin
      ) {
        cpg.method.fullNameExact("test::__construct").l.head.dominates.nonEmpty shouldBe true
      }
    }

    s"cpg post dominator for PHP $version" should {
      "work with function definition and the method should not post dominate any node" in new CpgFromCodeTestFixture(
        """function test() {
          |  echo "test";
          |}
          |""".stripMargin
      ) {
        cpg.method("test").head.postDominates.l.map(_.code).toSet shouldBe Set()
      }

      "have trivial post domination tree" in new CpgFromCodeTestFixture(
        """echo "test";""".stripMargin
      ) {
        cpg.method("dlr_main").head.postDominates.l.map(_.code).toSet shouldBe
          Set()
        val echo: Call = cpg.call.codeExact("ECHO string(\"test\")").head
        echo.dominates.l.map(_.code).toSet shouldBe
          Set("1", "RETURN int(1)", "<empty>")
      }

      "work with an if clause post" in new CpgFromCodeTestFixture(
        """if($x == true) {
          |   echo "true";
          |} else {
          |   echo "false";
          |}
          |echo "finally";
          |""".stripMargin
      ) {
        val echoTrue: Call = cpg.call.codeExact("ECHO string(\"true\")").head
        // echoTrue does only post dominate its parameter as the following node is a conditional jmp
        echoTrue.postDominates.map(_.code).l.toSet shouldBe Set("true")
        val jmp: Call = cpg.call.codeExact("JMP int(5)").head
        jmp.postDominates.map(_.code).l.toSet shouldBe
          Set("true", "ECHO string(\"true\")", "5")
      }
      "succeed on more complex example" in new CpgFromCodeTestFixture(
        """do {
          |  //echo "ttest";
          |} while(true);
          |""".stripMargin
      ) {
        //implicit val method: String = "test"
        //cfgSuccOf("JMPNZ bool(true) int(7)") shouldBe expectedCfg(("ttest", AlwaysEdge))
      }
    }
  }
}
