package io.joern.bytecode.passes

import io.joern.bytecode.parser.{PHPVersion, PHPVersions}
import io.joern.bytecode.util.unittesting.CpgFromCodeTestFixture
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DataDependencyPassTest extends AnyWordSpec with Matchers with PHPVersions {

  for(v <- getPhpVersions) {
    implicit val version: PHPVersion.Value = v

    s"DDG for PHP $version" should {
      "work for simple magic string" in new CpgFromCodeTestFixture(
        """
          |$x = "$a $b $c";
          |""".stripMargin
      ) {
        implicit val method: String = "dlr_main"
        ddgSuccOf("T5 = ROPE_INIT int(5) CV($a)") shouldBe expectedDdg(
          ("ROPE_ADD int(1) T5 string(\" \")", "T5"))
        ddgSuccOf("ROPE_ADD int(1) T5 string(\" \")") shouldBe Set()
        ddgSuccOf("T5 = ROPE_ADD int(1) T5 string(\" \")") shouldBe expectedDdg(
          ("ROPE_ADD int(2) T5 CV($b)", "T5"))
      }
      "$this->x" in new CpgFromCodeTestFixture(
        """class A
          |{
          |    function foo()
          |    {
          |        $this->a = 1;
          |        echo $this->a;
          |    }
          |}
          |(new A())->foo();""".stripMargin)(getPhpVersions.last) {
        implicit val method: String = "a::foo"
        ddgSuccOf("ASSIGN_OBJ THIS string(\"a\")") shouldBe expectedDdg(("FETCH_OBJ_R THIS string(\"a\")", "a"))
        ddgSuccOf("T1 = FETCH_OBJ_R THIS string(\"a\")") shouldBe expectedDdg(("ECHO T1", "T1"))
      }
      // we don't support this case, but we shouldn't crash :)
      "don't crash with $this->$x" in new CpgFromCodeTestFixture(
        """class A
          |{
          |    function foo()
          |    {
          |        $x = $_GET["a"];
          |        $this->$x = 1;
          |        echo $this->$x;
          |    }
          |}
          |(new A())->foo();""".stripMargin)(getPhpVersions.last) {
        implicit val method: String = "a::foo"
        ddgSuccOf("T5 = FETCH_OBJ_R THIS CV($x)") shouldBe expectedDdg(("ECHO T5", "T5"))
      }
      "edge between ASSIGN and ASSIGN_DIM" in new CpgFromCodeTestFixture(
        """$x = $array;
          |$x['foo'] = 42;
          |""".stripMargin) {
        implicit val method: String = "dlr_main"
        ddgSuccOf("ASSIGN CV($x) CV($array)") shouldBe expectedDdg(
          ("""ASSIGN_DIM CV($x) string("foo")""", "x")
        )
      }
      "ADD_ARRAY_ELEMENT missing data dependency edge" in new CpgFromCodeTestFixture(
        """$x = [
          |   'f' => 2,
          |   'h' => 2,
          |   'g' => (object) [ 'id' => 1 ],
          |   'j' => 2,
          |];
          |extract($x);
          |""".stripMargin
      ) {
        implicit val method: String = "dlr_main"
        ddgSuccOf("""INIT_ARRAY int(4) int(2) string("f")""") shouldBe expectedDdg(
          ("""ADD_ARRAY_ELEMENT int(2) string("h")""", "T1")
        )
        ddgSuccOf("""ADD_ARRAY_ELEMENT T2 string("g")""") shouldBe expectedDdg(
          ("""ADD_ARRAY_ELEMENT int(2) string("j")""", "T1")
        )
        //new CpgDotFileCreator(cpg.graph).show()
      }
    }
  }
}
