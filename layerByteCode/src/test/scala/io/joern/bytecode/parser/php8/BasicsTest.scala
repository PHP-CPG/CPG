package io.joern.bytecode.parser.php8

import fastparse._
import io.joern.bytecode.parser.php8.Basics._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BasicsTest extends AnyWordSpec with Matchers {

  "parser escapedSlash" should {
    "be able to parse \\" in {
      val str = "\\"
      val Parsed.Success(_, length) = parse(str, escapedSlash(_))
      length shouldBe str.length
    }
  }

  "parser legalString" should {
    "be able to parse single line string \"test\"" in {
      val singleLineString = "\"test\""
      val Parsed.Success(_, length) = parse(singleLineString, legalString(_))
      length shouldBe singleLineString.length
    }
    "be able to parse multi line string" in {
      val multiLineString =
        """"this is
          a multi line
          string""""
      val Parsed.Success(_, length) = parse(multiLineString, legalString(_))
      length shouldBe multiLineString.length
    }
    "be able to parse a bunch of spaces" in {
      val bunchOfSpaces = "\"      \""
      val Parsed.Success(_, length) = parse(bunchOfSpaces, legalString(_))
      length shouldBe bunchOfSpaces.length
    }
    "be able to parse multi line string with \"" in {
      val multiLineString =
        "\"this is \n a multi line string with \\\" \n in the middle\""
      val Parsed.Success(_, length) = parse(multiLineString, legalString(_))
      length shouldBe multiLineString.length
    }
    "be able to parse string with escaped slash" in {
      val str = "\"f\\s\""
      val Parsed.Success(_, length) = parse(str, legalString(_))
      length shouldBe str.length
    }
    "be able to parse fully qualified names" in {
      val str = "\"fully\\qualified\\name\""
      val Parsed.Success(_, length) = parse(str, legalString(_))
      length shouldBe str.length
    }
    "be able to parse PleskX\\Api\\strtolower" in {
      val str = "\"PleskX\\Api\\strtolower\""
      val Parsed.Success(_, length) = parse(str, legalString(_))
      length shouldBe str.length
    }
    "be able to parse <" in {
      val str = "\"<\""
      val Parsed.Success(_, length) = parse(str, legalString(_))
      length shouldBe str.length
    }
    "be able to parse /^[a-z]/" in {
      val str = "\"/^[a-z]/\""
      val Parsed.Success(_, length) = parse(str, legalString(_))
      length shouldBe str.length
    }
  }

}
