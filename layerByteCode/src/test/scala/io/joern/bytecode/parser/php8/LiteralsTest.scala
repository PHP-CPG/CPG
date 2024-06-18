package io.joern.bytecode.parser.php8

import fastparse._
import io.joern.bytecode.parser.EasyBase64.encode
import io.joern.bytecode.parser.constructs.{ArrayValue, ByteCodeKeyword, FloatLiteral}
import io.joern.bytecode.parser.php8.Literals._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class LiteralsTest extends AnyWordSpec with Matchers {

  "parser parseStringLiteral" should {
    "be able to parse string(\"value\")" in {
      val Parsed.Success(result, _) =
        parse(s"""string("${encode("value")}")""", getStringLiteral(_))
      assert(result.value == "value")
    }
    "be able to parse a string(\"value\") 42" in {
      val Parsed.Success(result, _) = parse(s"""string("${encode("value")}") 42""", getStringLiteral(_))
      result.value shouldBe "value"
    }
    "be able to parse string(\"a\")" in {
      val Parsed.Success(result, _) =
        parse(s"""string("${encode("a")}")""", getStringLiteral(_))
      assert(result.value == "a")
    }
    "be able to parse multi line strings without quotations" in {
      val multiLineString =
        """I am a
           a multi line string without
           quotation that should be parsable"""
      val stringLiteral = "string(\"" + encode(multiLineString) + "\")"
      val Parsed.Success(result, length) =
        parse(stringLiteral, getStringLiteral(_))
      assert(length == stringLiteral.length)
      assert(result.value == multiLineString)
    }
    "be able to parse multi line strings with escaped quotations" in {
      val multiLineString =
        "I am a \n multi line string with \\\" \n that should be parsable"
      val stringLiteral = "string(\"" + encode(multiLineString) + "\")"
      val Parsed.Success(result, length) =
        parse(stringLiteral, getStringLiteral(_))
      assert(length == stringLiteral.length)
      assert(result.value == multiLineString)
    }
    "be able to parse valid qualifed namespace path" in {
      val string = s"""string("${encode("PleskX\\Api\\strtolower")}")"""
      val Parsed.Success(result, length) = parse(string, getStringLiteral(_))
      length shouldBe string.length
      result.value shouldBe "PleskX\\Api\\strtolower"
    }
    "be able to parse string with underscore" in {
      val string = s"""string("${encode("some_text")}")"""
      val Parsed.Success(result, length) = parse(string, getStringLiteral(_))
      length shouldBe string.length
      result.value shouldBe "some_text"
    }
    "be able to parse string with leading underscore" in {
      val string = s"""string("${encode("_leading")}")"""
      val Parsed.Success(result, length) = parse(string, getStringLiteral(_))
      length shouldBe string.length
      result.value shouldBe "_leading"
    }
    "be able to parse string with numbers" in {
      val string = s"""string("${encode("Copyright3_6_56")}")"""
      val Parsed.Success(result, length) = parse(string, getStringLiteral(_))
      length shouldBe string.length
      result.value shouldBe "Copyright3_6_56"
    }
    "be bale to parse string that is empty" in {
      val string = s"""string("${encode("")}")"""
      val Parsed.Success(result, length) = parse(string, getStringLiteral(_))
      length shouldBe string.length
      result.value shouldBe ""
    }
    "be able to parse string string(\"/[\\s]+/\")" in {
      val string = s"""string("${encode("/[\\s]+/")}")"""
      val Parsed.Success(result, length) = parse(string, getStringLiteral(_))
      length shouldBe string.length
      result.value shouldBe "/[\\s]+/"
    }
    "be able to parse string string(\"Firebase\\JWT\\JWT\")" in {
      val string = s"""string("${encode("Firebase\\JWT\\JWT")}")"""
      val Parsed.Success(result, length) = parse(string, getStringLiteral(_))
      length shouldBe string.length
      result.value shouldBe "Firebase\\JWT\\JWT"
    }
    "be able to parse escaped backslash" in {
      val string = s"""string("${encode("\\\\")}")"""
      val value = "\\\\"
      val Parsed.Success(result, length) = parse(string, getStringLiteral(_))
      length shouldBe string.length
      result.value shouldBe value
    }
  }

  "parser parseIntLiteral" should {
    "be able to parse int(42)" in {
      val Parsed.Success(result, _) = parse("int(42)", getIntLiteral(_))
      assert(result.value == 42)
    }
    "be able to parse int(-42)" in {
      val Parsed.Success(result, _) = parse("int(-42)", getIntLiteral(_))
      result.value shouldBe -42
    }
    "be bale to parse int(0)" in {
      val Parsed.Success(result, _) = parse("int(0)", getIntLiteral(_))
      result.value shouldBe 0
    }
  }

  "parser parseFloatLiteral" should {
    "be able to parse float(1.2)" in {
      val Parsed.Success(result, _) = parse("float(1.2)", getFloatLiteral(_))
      result shouldBe FloatLiteral("1.2".toFloat)
    }
    "be bale to parse float(1)" in {
      val Parsed.Success(result, _) = parse("float(1)", getFloatLiteral(_))
      result shouldBe FloatLiteral("1".toFloat)
    }
    "be able to parse float(-1)" in {
      val Parsed.Success(result, _) = parse("float(-1)", getFloatLiteral(_))
      result shouldBe FloatLiteral("-1".toFloat)
    }
    "be able to parse float(-1.22)" in {
      val Parsed.Success(result, _) = parse("float(-1.22)", getFloatLiteral(_))
      result shouldBe FloatLiteral("-1.22".toFloat)
    }
    "be able to parse float(1e-06)" in {
      val Parsed.Success(result, _) = parse("float(1e-06)", getFloatLiteral(_))
      result shouldBe FloatLiteral("1e-06".toFloat)
    }
    "be able to parse float(1e+06)" in {
      val Parsed.Success(result, _) = parse("float(1e+06)", getFloatLiteral(_))
      result shouldBe FloatLiteral("1e+06".toFloat)
    }
    "be able to parse float(6.367e+06)" in {
      val Parsed.Success(result, _) = parse("float(6.367e+06)", getFloatLiteral(_))
      result shouldBe FloatLiteral("6.367e+06".toFloat)
    }
    "be able to process float(-inf)" in {
      val Parsed.Success(result, _) = parse("float(-inf)", getFloatLiteral(_))
      result shouldBe FloatLiteral(Float.NegativeInfinity)
    }
  }

  "parser array" should {
    "be able to parse empty arrays" in {
      val Parsed.Success(result, length) = parse("array()", getAnyLiteral(_))
      length shouldBe "array()".length
      result shouldBe ArrayValue(Some(List()))
    }
  }

  "(require_once)" should {
    "be able to parsed by getByteCodeKeyword" in {
      val string = "(require_once)"
      val Parsed.Success(result, length) = parse(string, getByteCodeKeyword(_))
      length shouldBe string.length
      result match {
        case ByteCodeKeyword(value) => value shouldBe "require_once"
      }
    }
  }

  "(require)" should {
    "be able to parsed by getByteCodeKeyword" in {
      val string = "(require)"
      val Parsed.Success(result, length) = parse(string, getByteCodeKeyword(_))
      length shouldBe string.length
      result match {
        case ByteCodeKeyword(value) => value shouldBe "require"
      }
    }
  }

  "getByteCodeKeyword" should {
    "be able to parse (require_once)" in {
      val string = "(require_once)"
      val Parsed.Success(result, length) = parse(string, getByteCodeKeyword(_))
      length shouldBe string.length
      result match {
        case ByteCodeKeyword(value) => value shouldBe "require_once"
      }
    }
    "be able to parse (require)" in {
      val string = "(require)"
      val Parsed.Success(result, length) = parse(string, getByteCodeKeyword(_))
      length shouldBe string.length
      result match {
        case ByteCodeKeyword(value) => value shouldBe "require"
      }
    }
  }

}
