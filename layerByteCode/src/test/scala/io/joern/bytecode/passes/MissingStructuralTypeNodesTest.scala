package io.joern.bytecode.passes

import io.joern.bytecode.parser.{PHPVersion, PHPVersions}
import io.joern.bytecode.util.unittesting.CpgFromCodeTestFixture
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MissingStructuralTypeNodesTest
    extends AnyWordSpec
    with Matchers with PHPVersions {

  for(v <- getPhpVersions) {
    implicit val version: PHPVersion.Value = v

    val fixture: CpgFromCodeTestFixture = CpgFromCodeTestFixture(
      """class foo {
        |   function bar() {
        |     echo "test";
        |   }
        |}
        |""".stripMargin)

    s"cpg for PHP $version" should {
      "have the typeDecl foo" in {
        fixture.cpg.typeDecl.toList.length shouldBe 1
      }
      "have the type foo, string, and NULL" in {
        fixture.cpg.typ.toList.length shouldBe 3
      }
    }
  }

}
