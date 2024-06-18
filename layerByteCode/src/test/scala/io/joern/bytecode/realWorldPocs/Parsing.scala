package io.joern.bytecode.realWorldPocs


import io.joern.bytecode.parser
import io.joern.bytecode.parser.PHPVersion.V8
import io.joern.bytecode.parser.utils.encodeBase64
import io.joern.bytecode.parser.{EasyBase64, PHPVersion, PHPVersions}
import io.joern.bytecode.util.implicits.OneableSeq
import io.joern.bytecode.util.unittesting.CpgFromCodeTestFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Expression}
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Parsing extends AnyWordSpec with Matchers with PHPVersions {

  for (v <- getPhpVersions) {
    implicit val version: parser.PHPVersion.Value = v
    s"PHP $v" should {
      "quotes in switch case" in new CpgFromCodeTestFixture(
        """$i = $_GET["a"];
          |
          |switch ($i) {
          |    case "'":
          |        echo 1;
          |        break;
          |    case "\"":
          |        echo 1;
          |        break;
          |    case "übel":
          |        echo "2";
          |    default:
          |        break;
          |}""".stripMargin) {
        val switch: Call = cpg.call("SWITCH_STRING").head
        val args: List[Expression] = switch.argument.l
        args.head.code shouldBe "CV($i)"
        args(1).code shouldBe "'"
        args(2).code shouldBe "11"
        args(3).code shouldBe "\""
        args(4).code shouldBe "13"
        args(5).code shouldBe "übel"
        args(6).code shouldBe "15"
        args(7).code shouldBe "default"
        args(8).code shouldBe "16"
      }
      "parse Null Coalescing Assignment Operator with arrays" in new CpgFromCodeTestFixture(
        """
          |$x[$y['z']] ??= [];
          |""".stripMargin) {
        // just don't fail.
      }
      /*
        * old bug java.lang.NumberFormatException
        * because the Class begins with BB we mistook it for a building block identifier (e.g. BB1) and the parse took the wrong route
        */
      "handle Classnames starting with BB" in new CpgFromCodeTestFixture(
        """
          |class BBMessageContent {
          |	  public function build() {}
          | }
          |""".stripMargin) {
        assert(true)
      }

      /*
      old bug with exception table parsing
       */
      "nested try/catch" in new CpgFromCodeTestFixture(
        """
          |try {
          |    echo 1;
          |    try {
          |        echo 2;
          |    } catch (Exception $e) {
          |        echo 3;
          |    }
          |} finally {
          |    echo 4;
          |}
          |""".stripMargin) {
        assert(true)
      }
    }
  }

  implicit val version: PHPVersion.Value = V8
  "quotes in match case" in new CpgFromCodeTestFixture(
    """match ($test) {
      |    "'" => "first",
      |    "\"" => "second",
      |    "übel" => "third",
      |    12 => "fourth",
      |};""".stripMargin) {
    val match_call: Call = cpg.call("MATCH").head
    val args: List[Expression] = match_call.argument.l
    args.head.code shouldBe "CV($test)"
    args(1).code shouldBe "'"
    args(2).code shouldBe "2"
    args(3).code shouldBe "\""
    args(4).code shouldBe "4"
    args(5).code shouldBe "übel"
    args(6).code shouldBe "6"
    args(7).code shouldBe "12"
    args(8).code shouldBe "8"
    args(9).code shouldBe "default"
    args(10).code shouldBe "1"
  }
  /*
  this was an old bug in the delete unreachable code pass.
   */
  "don't crash with nested arrow funcs" in new CpgFromCodeTestFixture("""fn($x) => fn($y) => 1;""".stripMargin) {
    assert(true)
  }
  "content of const arrays" in new CpgFromCodeTestFixture(
    """
      |$x = array(1,2,"foo" => array(42,6));
      |""".stripMargin) {
    val assign: Call = cpg.call("ASSIGN").l.one
    val b64: String = encodeBase64("""{"0":1,"1":2,"foo":"PHP2CPG-NESTED-ARRAY-LIMITATION"}""")
    assign.code shouldBe s"ASSIGN CV($$x) array($b64)"
    val args = assign.argument.l
    args(0).code shouldBe "CV($x)"
    args(1).code shouldBe s"array($b64)"
  }
  "empty array" when {
    "as variable init " in new CpgFromCodeTestFixture(
      """$x = [];""".stripMargin) {
      cpg.call("ASSIGN").l match {
        case assign :: Nil => assign.argument.code.l shouldBe List("CV($x)", "array()")
        case Nil => fail()
      }
    }
    "default value of function" in new CpgFromCodeTestFixture(
      """function f($x = []) {  }""".stripMargin) {
      cpg.method("f") should not be empty
      cpg.call("RECV_INIT").argument.order(1).code.head shouldBe "array()"
    }
  }
}

