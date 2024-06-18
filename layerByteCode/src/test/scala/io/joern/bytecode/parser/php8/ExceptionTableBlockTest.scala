package io.joern.bytecode.parser.php8

import fastparse._
import io.joern.bytecode.parser.php8.ExceptionTableBlock._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ExceptionTableBlockTest extends AnyWordSpec with Matchers {

  "parser getExceptionTableLine" should {
    "be able to parse '  0001, 0002, -, -'" in {
      val line = "  0001, 0002, -, -\n"
      val Parsed.Success(result, length) = parse(line, getExceptionTableLine(_))
      assert(length == line.length)
      assert(result.content.head == "0001")
      assert(result.content(1) == "0002")
      assert(result.content(2) == "-")
      assert(result.content(3) == "-")
    }
    "be able to parse '  BB1, BB2, -, -'" in {
      val line = "  BB1, BB2, -, -\n"
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
            0006, 0004, -, -
            0011, 0033, -, -
"""
      val Parsed.Success(result, length) =
        parse(block, getExceptionTableBlock(_))
      assert(length == block.length)
      assert(result.tableEntry.length == 2)
      assert(result.tableEntry.head.content.head == "0006")
      assert(result.tableEntry.head.content(1) == "0004")
      assert(result.tableEntry(1).content.head == "0011")
      assert(result.tableEntry(1).content(1) == "0033")
    }

    "be able to parse this example exception table block" in {
      val block =
        """EXCEPTION TABLE:
          |        0006, 0017, -, -
""".stripMargin
      val Parsed.Success(result, length) =
        parse(block, getExceptionTableBlock(_))
      length shouldBe block.length
      result.tableEntry.length shouldBe 1
    }
    "be able to parse this nested exception table block" in {
      val block =
        """EXCEPTION TABLE:
          |     0000, -, 0008, 0009     0001, 0003, -, -
""".stripMargin
      val Parsed.Success(result, length) =
        parse(block, getExceptionTableBlock(_), verboseFailures = true)
      length shouldBe block.length
      result.tableEntry.length shouldBe 1
    }
    "be able to parse this triple nested exception table block" in {
      val block =
        """EXCEPTION TABLE:
          |     0000, -, 0012, 0013     0001, 0007, -, -
          |     0001, 0003, -, -
""".stripMargin
      val Parsed.Success(result, length) =
        parse(block, getExceptionTableBlock(_), verboseFailures = true)
      length shouldBe block.length
      result.tableEntry.length shouldBe 2
    }
  }
}
