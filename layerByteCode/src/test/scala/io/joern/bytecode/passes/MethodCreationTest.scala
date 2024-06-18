package io.joern.bytecode.passes

import io.joern.bytecode.Defines
import io.joern.bytecode.parser.{PHPVersion, PHPVersions}
import io.joern.bytecode.util.unittesting.CpgFromCodeTestFixture
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MethodCreationTest extends AnyWordSpec with Matchers with PHPVersions {

  for(v <- getPhpVersions) {
    implicit val version: PHPVersion.Value = v

    val fixture: CpgFromCodeTestFixture = CpgFromCodeTestFixture(
      """
        |function test($first, $second) {
        | echo "test";
        |}
        |
        |function noParam() {
        | echo "no param";
        |}
        |""".stripMargin)

    s"cpg for PHP $version" should {
      "not have a single node with an order less than 0" in {
        fixture.cpg.method.foreach(
          method =>
            method.ast.foreach(
              node =>
                assert(
                  node.order >= 0,
                  s"node $node has the order ${node.order} which is below zero")
            )
        )
      }
      "have a single test method definition" in {
        val method = fixture.cpg.method("test").l
        method.length shouldBe 1
        method.head.name shouldBe "test"
        method.head.code shouldBe "test($param1, $param2)"
      }
      "have a single noParam method definition" in {
        //remember function names are case insensitive and as such stored lower case in the cpg
        val methods = fixture.cpg.method("noparam").l
        methods match {
          case method :: Nil =>
            method.name shouldBe "noparam"
            method.code shouldBe "noparam()"
          case x => fail(s"unexpected traversal result $x")
        }
      }
      "have a single namespace node" in {
        fixture.cpg.namespaceBlock(Defines.GLOBAL_NAMESPACE_NAME).l match {
          case namespace :: Nil =>
            namespace.name shouldBe Defines.GLOBAL_NAMESPACE_NAME
          case x => fail(s"unexpected traversal result $x")
        }
      }

      "create a method with the name of the class" in new CpgFromCodeTestFixture(
        """class Klasse {
          |   public function Send() {
          |      echo "test";
          |   }
          |}
          |""".stripMargin
      ) {
        cpg.method.fullName("klasse::send").toList.size shouldBe 1
      }
    }
  }
}
