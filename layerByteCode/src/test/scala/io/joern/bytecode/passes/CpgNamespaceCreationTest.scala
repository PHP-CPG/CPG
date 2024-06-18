package io.joern.bytecode.passes

import io.joern.bytecode.parser.{PHPVersion, PHPVersions}
import io.joern.bytecode.util.unittesting.CpgFromCodeTestFixture
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, nodes}
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.jdk.CollectionConverters._

class CpgNamespaceCreationTest
    extends AnyWordSpec
    with Matchers with PHPVersions  {

  for(v <- getPhpVersions) {
    implicit val version: PHPVersion.Value = v

    val fixture: CpgFromCodeTestFixture = CpgFromCodeTestFixture(
      """
        |namespace weird\testOne;
        |
        |function notAMethod() {
        |    echo("test");
        |}
        |
        |class test {
        |
        |    function test() {
        |        echo("test");
        |    }
        |
        |}
        |
        |namespace testTwo;
        |
        |function notAMethod() {
        |    echo("test");
        |}
        |
        |class test {
        |
        |    function test() {
        |        echo("test");
        |    }
        |
        |}
        |
        |use weird\testOne;
        |
        |testOne\notAMethod();
        |notAMethod();
        |""".stripMargin
    )

    s"CPG namespace structure for PHP $version" should {
      "have namespace weird\\testone" in {
        fixture.cpg.namespaceBlock.nameExact("weird\\testone").l match {
          case _ :: Nil =>
          case x => fail(s"unexpected traversal result $x")
        }
      }
      "have namespace testtwo" in {
        fixture.cpg.namespaceBlock.nameExact("testtwo").l match {
          case _ :: Nil =>
          case x => fail(s"unexpected traversal result $x")
        }
      }
    }

    s"namespace testtwo for PHP $version" should {
      "have a function notamethod" in {
        fixture.cpg.method.fullNameExact("testtwo\\notamethod").l match {
          case (method: nodes.Method) :: Nil =>
            method.name shouldBe "notamethod"
            method.in(EdgeTypes.CALL).asScala.toList match {
              case (single: nodes.Call) :: Nil =>
                single.code shouldBe "DO_FCALL_BY_NAME"
              case Nil => fail("expected call edge not found")
              case x => fail(s"unexpected traversal result $x")
            }
          case x => fail(s"unexpected traversal result $x")
        }
      }
      "have a typedecl test with method test" in {
        fixture.cpg.typeDecl.fullNameExact("testtwo\\test").l match {
          case (typeDecl: nodes.TypeDecl) :: Nil =>
            typeDecl.name shouldBe "test"
            typeDecl.astChildren.isMethod.l match {
              case method :: Nil =>
                method.fullName shouldBe "testtwo\\test::test"
                method.name shouldBe "test"
              case x => fail(s"unexpected traversal result $x")
            }
          case x => fail(s"unexpected traversal result $x")
        }
      }
    }

    s"namespace weird\\testone for PHP $version" should {
      "have a function notamethod" in {
        fixture.cpg.method.fullNameExact("weird\\testone\\notamethod").l match {
          case (method: nodes.Method) :: Nil =>
            method.name shouldBe "notamethod"
            method.in(EdgeTypes.CALL).asScala.toList match {
              case (single: nodes.Call) :: Nil =>
                single.code shouldBe "DO_UCALL"
              case Nil => fail("expected call edge not found")
              case x => fail(s"unexpected traversal result $x")
            }
          case x => fail(s"unexpected traversal result $x")
        }
      }
      "have a typedecl test with method test" in {
        fixture.cpg.typeDecl.fullNameExact("weird\\testone\\test").l match {
          case (typeDecl: nodes.TypeDecl) :: Nil =>
            typeDecl.name shouldBe "test"
            typeDecl.astChildren.isMethod.l match {
              case method :: Nil =>
                method.fullName shouldBe "weird\\testone\\test::test"
                method.name shouldBe "test"
              case x => fail(s"unexpected traversal result $x")
            }
          case x => fail(s"unexpected traversal result $x")
        }
      }
    }
  }

}
