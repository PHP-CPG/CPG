package io.joern.bytecode.passes

import io.joern.bytecode.parser.{PHPVersion, PHPVersions}
import io.joern.bytecode.util.unittesting.CpgFromCodeTestFixture
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DeclareClassDelayedTest extends AnyWordSpec with Matchers with PHPVersions {

  for(v <- getPhpVersions) {
    implicit val version: PHPVersion.Value = v

    s"cpg for PHP $version" should {
      "contain call to DECLARE_CLASS_DELAYED" in new CpgFromCodeTestFixture(
        """
          |class B extends A {
          |  function test2() {
          |     echo "test2";
          |  }
          |}
          |""".stripMargin
      ) {
        cpg.call("DECLARE_CLASS_DELAYED").l match {
          case single :: Nil =>
            single.name shouldBe "DECLARE_CLASS_DELAYED"
          case x => fail(s"unexpected traversal result $x")
        }
      }
    }

    s"cpg for PHP $version" should {
      "not contain call to DECLARE_CLASS_DELAYED" in new CpgFromCodeTestFixture(
        """
          |class A {
          |  function test2() {
          |     echo "test2";
          |  }
          |}
          |""".stripMargin
      ) {
        cpg.call("DECLARE_CLASS_DELAYED").l match {
          case Nil =>
          case x => fail(s"unexpected traversal result $x")
        }
      }
    }
  }
}
