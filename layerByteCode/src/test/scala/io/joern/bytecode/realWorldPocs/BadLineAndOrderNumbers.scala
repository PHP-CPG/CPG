package io.joern.bytecode.realWorldPocs

import io.joern.bytecode.parser.PHPVersion
import io.joern.bytecode.parser.PHPVersion.V7
import io.joern.bytecode.util.unittesting.CpgFromCodeTestFixture
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BadLineAndOrderNumbers extends AnyWordSpec with Matchers {

  // line numbers only matter for PHP7 as 8 does not provide them anymore
  implicit val version: PHPVersion.Value = V7

  "in PHP7 the line numbers and order" should {
    "be correct" in new CpgFromCodeTestFixture(
      s"""mysqli_query($$link,"BEGIN");
         |mysqli_query($$link,"SELECT test FROM ttable WHERE condition = true");
         |mysqli_query($$link,"UPDATE ttable SET test = '42' WHERE condition = true");
         |mysqli_query($$link,"COMMIT");
         |""".stripMargin
    ) {
      /*cpg.call.code("DO_FCALL_BY_NAME").map(_.lineNumber).map(_.get).toSet shouldBe Set(2,
                                                                         3,
                                                                         4,
                                                                         5)*/
      //this result set is derived by looking at the println - might be subject to change
      //relevant part of the test is that there are four different
      cpg.call.code("DO_FCALL_BY_NAME").map(_.order).toSet shouldBe Set(3, 7, 11, 15)
    }
  }
}
