package io.joern.php

import io.joern.TraversalUtils
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class AssignmentOperatorsTest
    extends AnyWordSpec
    with Matchers
    with TraversalUtils {

  override val fixture = CpgFromCodeTestFixture(
    """$test += 1;
      |$test -= 1;
      |$test *= 1;
      |$test %= 1;
      |$test /= 1;
      |$test .= "test";
      |$test &= $test;
      |$test |= $test;
      |$test ^= $test;
      |$test <<= $test;
      |$test >> $test;
      |$test ??= $test;
      |""".stripMargin
  )

  "cpg" should {
    "have a /= call" in {
      val call = fixture.cpg.call("/=").l
      call.length shouldBe 1
      call.head.name shouldBe "/="
    }
    "have a *= call" in {
      val call = fixture.cpg.call("\\*=").l
      call.length shouldBe 1
      call.head.name shouldBe "*="
    }
    "have a += call" in {
      val call = fixture.cpg.call("\\+=").l
      call.length shouldBe 1
      call.head.name shouldBe "+="
    }
    "have a -= call" in {
      val call = fixture.cpg.call("-=").l
      call.length shouldBe 1
      call.head.name shouldBe "-="
    }
    "have a %= call" in {
      val call = fixture.cpg.call("%=").l
      call.length shouldBe 1
      call.head.name shouldBe "%="
    }
  }

}
