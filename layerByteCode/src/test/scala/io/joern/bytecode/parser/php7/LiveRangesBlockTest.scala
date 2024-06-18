package io.joern.bytecode.parser.php7

import fastparse._
import io.joern.bytecode.parser.php7.LiveRangesBlock.getLiveRangesBlock
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class LiveRangesBlockTest extends AnyWordSpec with Matchers {

  "parser getLiveRangesBlock" should {
    "be able to parse a full live ranges block" in {
      val block =
        """LIVE RANGES:
           4: L2 - L5 (loop)
"""
      val Parsed.Success(result, length) = parse(block, getLiveRangesBlock(_))
      length shouldBe block.length
      result.rangesEntry.length shouldBe 1
      result.rangesEntry.head.varNum shouldBe 4
      result.rangesEntry.head.start shouldBe 2
      result.rangesEntry.head.end shouldBe 5
      result.rangesEntry.head.rangeType shouldBe "loop"
    }
    "be able to parse valid block" in {
      val block =
        """LIVE RANGES:
          |        3: L4 - L19 (loop)
          |""".stripMargin
      val Parsed.Success(_, count) = parse(block, getLiveRangesBlock(_))
      count shouldBe block.length
    }
  }

}
