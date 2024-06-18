package io.joern.php

import io.joern.TraversalUtils
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ClassAndMethodDefinitionTest
    extends AnyWordSpec
    with Matchers
    with TraversalUtils {

  override val fixture: CpgFromCodeTestFixture = CpgFromCodeTestFixture(
    """class test extends otherClass {
      |
      |   private static $other = 33;
      |   private $attribute = 42;
      |
      |   const CONST = 'const';
      |
      |   public function __construct($test) {
      |     $this->attribute = 23;
      |   }
      |
      |   public function test($default = 42) {
      |     echo $this->attribute;
      |   }
      |
      |}
      |
      |$val = new test(33);
      |$val->test();
      |test::CONST;
      |""".stripMargin)

  "cpg" should {
    "have one type definition" in {
      val types = fixture.cpg.typeDecl.l
      types.length shouldBe 1
      types.head.name shouldBe "test"
    }
    "have two fetch calls" in {
      val fetchs = fixture.cpg.call("->").l
      fetchs.length shouldBe 2
      fetchs.head.astChildren.order(0).head match {
        case lhs: nodes.Identifier => lhs.name shouldBe "this"
      }
      fetchs.head.astChildren.order(1).head match {
        case rhs: nodes.Identifier => rhs.name shouldBe "attribute"
      }
    }
    "have a single method call" in {

    }
  }

}
