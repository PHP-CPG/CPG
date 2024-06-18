package io.joern.php

import io.joern.TraversalUtils
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class FunctionDefinitionTest
    extends AnyWordSpec
    with Matchers
    with TraversalUtils {

  override val fixture = CpgFromCodeTestFixture("""function test($var) {
      |    print_r($var);
      |    return $var + $var;
      |}
      |""".stripMargin)

  "cpg" should {
    "contain a single method called test" in {
      fixture.cpg.method("test").l.length shouldBe 1
    }
    "that method should have 2 substatements" in {
      fixture.cpg.method("test").astChildren.isBlock.l.length shouldBe 1
      fixture.cpg
        .method("test")
        .astChildren
        .isBlock
        .head
        .astChildren
        .l
        .length shouldBe 2
    }
  }
}
