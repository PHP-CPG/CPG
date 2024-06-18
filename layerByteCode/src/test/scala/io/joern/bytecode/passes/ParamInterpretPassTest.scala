package io.joern.bytecode.passes

import io.joern.bytecode.parser.{PHPVersion, PHPVersions}
import io.joern.bytecode.util.unittesting.CpgFromCodeTestFixture
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ParamInterpretPassTest extends AnyWordSpec with Matchers with PHPVersions {

  for (v <- getPhpVersions) {
    implicit val version: PHPVersion.Value = v

    s"paramInterpretPass for PHP $version" should {
      "contain correct name fields for parameters" in new CpgFromCodeTestFixture(
        """
          |echo "test";
          |function foo($x, $y, $z) {
          |}
          |
          |foo(1,2,3);
          |""".stripMargin
      ) {
        cpg.parameter.map(x => (x.order, x.name)).toSet shouldBe Set(
          (0, "x"),
          (1, "y"),
          (2, "z"))
      }
    }
  }
  "PHP 8 named params" when {
    "example" in new CpgFromCodeTestFixture(
      """function f($m, $e = "Hi Surf", $url = "http://www.example.org")
        |{
        |    echo $m;
        |    echo $e;
        |    return file_get_contents($url);
        |}
        |echo f(m: "Hello Surf", url: $_GET["a"]);
        |""".stripMargin
    )(PHPVersion.V8) {
      cpg.parameter.map(x => (x.order, x.name)).toSet shouldBe Set(
        (0, "m"),
        (1, "e"),
        (2, "url"))
    }
    "variation: usage swapped" in new CpgFromCodeTestFixture(
      """function f($m, $e = "Hi Surf", $url = "http://www.example.org")
        |{
        |    echo $e;
        |    echo $m;
        |    return file_get_contents($url);
        |}
        |echo f(m: "Hello Surf", url: $_GET["a"]);
        |""".stripMargin
    )(PHPVersion.V8) {
      cpg.parameter.map(x => (x.order, x.name)).toSet shouldBe Set(
        (0, "m"),
        (1, "e"),
        (2, "url"))
    }
    "variation: call site param order swapped" in new CpgFromCodeTestFixture(
      """function f($m, $e = "Hi Surf", $url = "http://www.example.org")
        |{
        |    echo $m;
        |    echo $e;
        |    return file_get_contents($url);
        |}
        |echo f(url: $_GET["a"], m: "Hello Surf");
        |""".stripMargin
    )(PHPVersion.V8) {
      cpg.parameter.map(x => (x.order, x.name)).toSet shouldBe Set(
        (0, "m"),
        (1, "e"),
        (2, "url"))
    }
    "variation: usage and call site param order swapped" in new CpgFromCodeTestFixture(
      """function f($m, $e = "Hi Surf", $url = "http://www.example.org")
        |{
        |    echo $e;
        |    echo $m;
        |    return file_get_contents($url);
        |}
        |echo f(url: $_GET["a"], m: "Hello Surf");
        |""".stripMargin
    )(PHPVersion.V8) {
      cpg.parameter.map(x => (x.order, x.name)).toSet shouldBe Set(
        (0, "m"),
        (1, "e"),
        (2, "url"))
    }

  }
}
