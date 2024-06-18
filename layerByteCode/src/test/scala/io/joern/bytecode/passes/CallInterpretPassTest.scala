package io.joern.bytecode.passes

import io.joern.bytecode.parser
import io.joern.bytecode.parser.PHPVersions
import io.joern.bytecode.util.unittesting.CpgFromCodeTestFixture
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CallInterpretPassTest extends AnyWordSpec with Matchers with PHPVersions {

  for(v <- getPhpVersions) {
    implicit val version: parser.PHPVersion.Value = v

    s"call for PHP $version" should {
      "should have a CALL node with name=`foo`" in new CpgFromCodeTestFixture(
        """
          | function foo() {
          |
          | }
          |
          | foo();
          |""".stripMargin) {
        val List(x) = cpg.call("foo").l
        x.name shouldBe "foo"
        x.code shouldBe "DO_UCALL"
      }
    }
  }
}
