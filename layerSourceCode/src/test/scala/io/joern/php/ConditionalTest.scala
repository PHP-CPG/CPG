package io.joern.php

import io.joern.{AbstractCpgTestFixture, TraversalUtils}
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ConditionalTest extends AnyWordSpec with Matchers with TraversalUtils {

  val fixture: AbstractCpgTestFixture = CpgFromCodeTestFixture(
    """if($test) {
      |  echo 'a';
      |} elseif ($test == 3) {
      |  echo 3;
      |} elseif ($test == 4) {
      |  echo 4;
      |} else {
      |  echo 'b';
      |}
      |""".stripMargin)

  "cpg" should {
    "have two calls" in {
      // four echo and two == (equal)
      fixture.cpg.call.l.length shouldBe 6
    }
    "have a three control structure" in {
      // one if and two elseif
      fixture.cpg.controlStructure.l.length shouldBe 3
    }
    "have two control structures under the if control structure" in {
      fixture.cpg.controlStructure
        .code("if.*")
        .astChildren
        .isControlStructure
        .l
        .length shouldBe 2
    }
    "have two code blocks under the if control structure" in {
      fixture.cpg.controlStructure
        .code("if.*")
        .astChildren
        .isBlock
        .l
        .length shouldBe 2
    }
  }

}
