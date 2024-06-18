package io.joern.bytecode.realWorldPocs

import io.joern.bytecode.parser.PHPVersion
import io.joern.bytecode.parser.PHPVersion.V8
import io.joern.bytecode.util.unittesting.CpgFromCodeTestFixture
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class php81 extends AnyWordSpec with Matchers {

  implicit val version: PHPVersion.Value = V8

  "callable convert" in new CpgFromCodeTestFixture(
    """
      |$c = strlen(...);
      |echo c("asdasd");
      |""".stripMargin) {
    cpg.call("CALLABLE_CONVERT").nonEmpty shouldBe true
  }

  "never" in new CpgFromCodeTestFixture("""function f(): never {exit();}"""){
    cpg.all.nonEmpty shouldBe true
  }

  "can work with all php8.1 features at once" in new CpgFromCodeTestFixture(
    """
      |function f(): never
      |{
      |    $g = $GLOBALS;
      |    $a = $_GET;
      |    $c = strlen(...);
      |    $b = match ($a["b"] + 1) {
      |        1 => $o?->f(),
      |        2 => htmlspecialchars($string, double_encode: false),
      |        3 => $c("a"),
      |    };
      |    match ($a + 1) {
      |        2 => print(1),
      |    };
      |    if($ba?->do()){}
      |    exit(1);
      |}
      |f();
      |
      |""".stripMargin) {
    cpg.call("CALLABLE_CONVERT").nonEmpty shouldBe true
    cpg.call("MATCH").nonEmpty shouldBe true
    cpg.call("MATCH_ERROR").nonEmpty shouldBe true
    cpg.call("CHECK_UNDEF_ARGS").nonEmpty shouldBe true
    cpg.call("FETCH_GLOBALS").nonEmpty shouldBe true
    cpg.call("VERIFY_NEVER_TYPE").nonEmpty shouldBe true
    cpg.call("JMP_NULL").nonEmpty shouldBe true
  }

  "check func arg with string literal" in new CpgFromCodeTestFixture("""call_user_func_array(a: $field["callback"], b: array($field));""".stripMargin) {
    // the code throws a runtime error
    // but we should be able to convert it
    assert(true)
  }
}
