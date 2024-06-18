package io.joern.bytecode.passes

import io.joern.bytecode.parser.{PHPVersion, PHPVersions}
import io.joern.bytecode.util.unittesting.CpgFromCodeTestFixture
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MissingStructuralNamespaceNodesTest
    extends AnyWordSpec
    with Matchers with PHPVersions {

  for(v <- getPhpVersions) {
    implicit val version: PHPVersion.Value = v

    val fixture: CpgFromCodeTestFixture = CpgFromCodeTestFixture(
      """namespace FooBarBaz;
        |
        |const TEST = 1;
        |class Foo {
        |   function bar() {
        |       global $stuff;
        |       $stuff = 42;
        |       $baz = 1337 + $stuff;
        |       return $baz;
        |   }
        |}
        |""".stripMargin)

    s"cpg for PHP $version" should {
      "have the namespace test" in {
        // fixture.cpg.namespace.toList.length shouldBe 1
      }
      "have a typeDecl named Foo" in {
        fixture.cpg.typeDecl.fullName("foobarbaz\\\\foo").l.length shouldBe 1
      }
      "have a method named bar" in {
        fixture.cpg.method.fullName("foobarbaz\\\\foo::bar").l.length shouldBe 1
      }
      "namespace member named TEST" in {
        fixture.cpg
          .namespaceBlock("foobarbaz")
          .astChildren
          .isMember
          .l
          .length shouldBe 1
      }
      "have local named baz" in {
        // should have two local has we have $baz and a temporary T2
        fixture.cpg
          .method.fullName("foobarbaz\\\\foo::bar")
          .head
          .astChildren
          .isLocal
          .l
          .length shouldBe 2
      }
      "have the type int" in {
        fixture.cpg.typ("foobarbaz\\\\foo").l.length shouldBe 1
        fixture.cpg.typ("Integer").l.length shouldBe 1
      }
    }
  }
}
