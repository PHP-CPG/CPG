package io.joern.bytecode.parser.php7

import fastparse._
import io.joern.bytecode.parser.php7.ExceptionTableBlock._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ExceptionTableBlockTest extends AnyWordSpec with Matchers {

  "parser getExceptionTableLine" should {
    "be able to parse '  L1, L2, -, -'" in {
      val line = "  L1, L2, -, -"
      val Parsed.Success(result, length) = parse(line, getExceptionTableLine(_))
      assert(length == line.length)
      assert(result.content.head == "1")
      assert(result.content(1)  == "2")
      assert(result.content(2)  == "-")
      assert(result.content(3)  == "-")
    }
    "be able to parse '  BB1, BB2, -, -'" in {
      val line = "  BB1, BB2, -, -"
      val Parsed.Success(result, length) = parse(line, getExceptionTableLine(_))
      assert(length == line.length)
      assert(result.content.head == "1")
      assert(result.content(1) == "2")
      assert(result.content(2) == "-")
      assert(result.content(3) == "-")
    }
  }

  "parser getExceptionTableBlock" should {
    "be able to parse a 00XX exception table block" in {
      val block =
        """EXCEPTION TABLE:
            L6, L4, -, -
            L11, L33, -, -
"""
      val Parsed.Success(result, length) =
        parse(block, getExceptionTableBlock(_))
      assert(length == block.length)
      assert(result.tableEntry.length == 2)
      assert(result.tableEntry.head.content.head == "6")
      assert(result.tableEntry.head.content(1) == "4")
      assert(result.tableEntry(1).content(0) == "11")
      assert(result.tableEntry(1).content(1) == "33")
    }

    "be able to parse this example exception table block" in {
      val block =
        """EXCEPTION TABLE:
          |        L6, L17, -, -
""".stripMargin
      val Parsed.Success(result, length) =
        parse(block, getExceptionTableBlock(_))
      length shouldBe block.length
      result.tableEntry.length shouldBe 1
    }
  }
}
