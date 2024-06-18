package io.joern.php

import io.joern.TraversalUtils
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SwitchCaseTest extends AnyWordSpec with Matchers with TraversalUtils {

  override val fixture = CpgFromCodeTestFixture("""switch($i) {
      |  case 1:
      |     echo "test";
      |     break;
      |  case 2:
      |     echo "otherTest";
      |  case true:
      |     echo "True";
      |     break;
      |  default:
      |     echo "default";
      |}
      |""".stripMargin)

  "cpg" should {
    "have no function" in {
      fixture.cpg.method.l.length shouldBe 0
    }
    "have a single switch" in {
      fixture.cpg.controlStructure.code("switch.*").l.length shouldBe 1
    }
    "the switch should have 4 case children" in {
      fixture.cpg.controlStructure
        .code("switch.*")
        .astChildren
        .isControlStructure
        .code("case.*")
        .l
        .length shouldBe 4
    }
  }
}
