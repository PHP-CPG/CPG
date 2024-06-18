package io.joern.bytecode.parser.php8

import fastparse._
import io.joern.bytecode.parser.php8.LiveRangesBlock.getLiveRangesBlock
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class LiveRangesBlockTest extends AnyWordSpec with Matchers {

  "parser getLiveRangesBlock" should {
    "be able to parse a full live ranges block" in {
      val block =
        """LIVE RANGES:
           4: 0002 - 0005 (loop)
"""
      val Parsed.Success(result, length) = parse(block, getLiveRangesBlock(_))
      length shouldBe block.length
      result.rangesEntry.length shouldBe 1
      result.rangesEntry.head.varNum shouldBe 4
      result.rangesEntry.head.start shouldBe 2
      result.rangesEntry.head.end shouldBe 5
      result.rangesEntry.head.rangeType shouldBe "loop"
    }
  }

}
