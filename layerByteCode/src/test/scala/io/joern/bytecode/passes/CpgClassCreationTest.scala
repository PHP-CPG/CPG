package io.joern.bytecode.passes

import io.joern.bytecode.parser
import io.joern.bytecode.parser.PHPVersions
import io.joern.bytecode.util.unittesting.CpgFromCodeTestFixture
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, nodes}
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.jdk.CollectionConverters._

class CpgClassCreationTest
    extends AnyWordSpec
    with Matchers with PHPVersions {

  for(v <- getPhpVersions) {
    implicit val version: parser.PHPVersion.Value = v

    val fixture: CpgFromCodeTestFixture = CpgFromCodeTestFixture(
      """
        |class Basic {
        |
        |     public $var = 42;
        |
        |     function __construct($val) {
        |         $var = $val;
        |     }
        |
        |     private function test($test) {
        |         echo $test;
        |     }
        |
        |     public function test2($test) {
        |         $this->test($rest);
        |         test($rest);
        |     }
        | }
        |
        | $var = new Basic("value");
        | $var->test2("other");
        |""".stripMargin
    )

    s"CPG for PHP $version" should {

      "have a single type declaration" in {
        fixture.cpg.typeDecl.l match {
          case typeDecl :: Nil =>
            typeDecl.name shouldBe "basic"
            typeDecl.fullName shouldBe "basic"
            typeDecl.astParentType shouldBe "NAMESPACE_BLOCK"
          case x => fail(s"unexpected traversal result $x")
        }
      }

      "the type decl should have three methods and a main" in {
        fixture.cpg.typeDecl.l match {
          case typeDecl :: Nil =>
            typeDecl.astChildren.isMethod.sortBy(_.name).toList match {
              case first :: second :: third :: Nil =>
                first.name shouldBe "__construct"
                first.fullName shouldBe "basic::__construct"
                second.name shouldBe "test"
                second.fullName shouldBe "basic::test"
                third.name shouldBe "test2"
                third.fullName shouldBe "basic::test2"
              case x => fail(s"unexpected traversal result $x")
            }
          case x => fail(s"unexpected traversal result $x")
        }
        fixture.cpg.method("dlr_main").l match {
          case main :: Nil =>
            main.name shouldBe "dlr_main"
          case x => fail(s"unexpected traversal result $x")
        }
      }

      "have 1 call edges for Basic::test2" in {
        fixture.cpg.method.fullName("basic::test2").l match {
          case method :: Nil =>
            method.in(EdgeTypes.CALL).asScala.toList match {
              case (caller: nodes.Call) :: Nil =>
                caller.code shouldBe "DO_FCALL"
              case Nil => fail("expected call edge not created")
              case x => fail(s"unexpected traversal result $x")
            }
          case x => fail(s"unexpected traversal result $x")
        }
      }

      "have 1 call edge for Basic::__construct" in {
        fixture.cpg.method.fullName("basic::__construct").l match {
          case method :: Nil =>
            method.in(EdgeTypes.CALL).asScala.toList match {
              case (caller: nodes.Call) :: Nil =>
                caller.code shouldBe "DO_FCALL"
              case x => fail(s"unexpected traversal result $x")
            }
          case x => fail(s"unexpected traversal result $x")
        }
      }
    }
  }
}
