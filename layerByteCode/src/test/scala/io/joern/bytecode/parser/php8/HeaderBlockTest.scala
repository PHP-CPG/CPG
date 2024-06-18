package io.joern.bytecode.parser.php8

import fastparse._
import io.joern.bytecode.parser.php8.HeaderBlock._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class HeaderBlockTest extends AnyWordSpec with Matchers {

   "parsing '$_main:\n  ; (lines=42, args=43, vars=44, tmps=45)'" should {
    "be parsable only considering name results" in {
      val Parsed.Success(_, success) =
        parse("$_main", parseHeaderBlockMethodIdentifier(_))
      assert(success == 6) //check that all 6 letters are consumed
    }
    "be parsable only considering meta variable results in" in {
      val Parsed.Success(result, _) =
        parse("(lines=42, args=43, vars=44, tmps=45)",
              parseHeaderBlockMethodMetaBlock(_))
      assert(result._1 == 42)
      assert(result._2 == 43)
      assert(result._3 == 44)
      assert(result._4 == 45)
    }
    "be completely parsable" in {
      val Parsed.Success(result, _) =
        parse("$_main:\n  ; (lines=42, args=43, vars=44, tmps=45)",
              getHeaderBLockMethodDefinitionLine(_))
      assert(result.name == "dlr_main")
      assert(result.lines == 42)
      assert(result.args == 43)
      assert(result.vars == 44)
      assert(result.tmps == 45)
    }
  }

  "parser getHeaderBlockMethodIdentifier" should {
    "be able to parse {closure}" in {
      val name = "{closure}"
      val Parsed.Success(result, length) =
        parse(name, getHeaderBlockMethodIdentifier(_))
      length shouldBe name.length
      result._1 shouldBe None
      result._2 shouldBe None
      result._3 shouldBe "{closure}"
    }
    "be able to parse $_main" in {
      val name = "$_main"
      val Parsed.Success(result, length) =
        parse(name, getHeaderBlockMethodIdentifier(_))
      length shouldBe name.length
      result._1 shouldBe None
      result._2 shouldBe None
      result._3 shouldBe "$_main"
    }
    "be able to parse testFunction" in {
      val name = "testFunction"
      val Parsed.Success(result, length) =
        parse(name, getHeaderBlockMethodIdentifier(_))
      length shouldBe name.length
      result._1 shouldBe None
      result._2 shouldBe None
      result._3 shouldBe "testFunction"
    }
    "be able to parse namespace\\testFunction" in {
      val name = "namespace\\testFunction"
      val Parsed.Success(result, length) =
        parse(name, getHeaderBlockMethodIdentifier(_))
      length shouldBe name.length
      result._1 shouldBe Some("namespace")
      result._2 shouldBe None
      result._3 shouldBe "testFunction"
    }
    "be able to parse weird\\namespace\\testFunction" in {
      val name = "weird\\namespace\\testFunction"
      val Parsed.Success(result, length) =
        parse(name, getHeaderBlockMethodIdentifier(_))
      length shouldBe name.length
      result._1 shouldBe Some("weird\\namespace")
      result._2 shouldBe None
      result._3 shouldBe "testFunction"
    }
    "be able to parse className::testFunction" in {
      val name = "className::testFunction"
      val Parsed.Success(result, length) =
        parse(name, getHeaderBlockMethodIdentifier(_))
      length shouldBe name.length
      result._1 shouldBe None
      result._2 shouldBe Some("className")
      result._3 shouldBe "testFunction"
    }
    "be able to parse namespace\\className::testFunction" in {
      val name = "namespace\\className::testFunction"
      val Parsed.Success(result, length) =
        parse(name, getHeaderBlockMethodIdentifier(_))
      length shouldBe name.length
      result._1 shouldBe Some("namespace")
      result._2 shouldBe Some("className")
      result._3 shouldBe "testFunction"
    }
    "be able to parse weird\\namespace\\className::testFunction" in {
      val name = "weird\\namespace\\className::testFunction"
      val Parsed.Success(result, length) =
        parse(name, getHeaderBlockMethodIdentifier(_))
      length shouldBe name.length
      result._1 shouldBe Some("weird\\namespace")
      result._2 shouldBe Some("className")
      result._3 shouldBe "testFunction"
    }
    "be able to parse long\\weird\\namespace\\className::testFunction" in {
      val name = "long\\weird\\namespace\\className::testFunction"
      val Parsed.Success(result, length) =
        parse(name, getHeaderBlockMethodIdentifier(_))
      length shouldBe name.length
      result._1 shouldBe Some("long\\weird\\namespace")
      result._2 shouldBe Some("className")
      result._3 shouldBe "testFunction"
    }
    "be able to parse \\strlen" in {
      val name = "\\strlen"
      val Parsed.Success(result, length) =
        parse(name, getHeaderBlockMethodIdentifier(_))
      length shouldBe name.length
      result._1 shouldBe None
      result._2 shouldBe None
      result._3 shouldBe "strlen"
    }
  }

  "parser getMetaLineParsing " should {
    "be able to parse '; (before optimizer)'" in {
      val Parsed.Success(result, _) =
        parse("; (before optimizer)", getHeaderBlockMetaLineParsing(_))
      assert(result.metaInfo.length == 2)
      assert(result.metaInfo(0) == "before")
      assert(result.metaInfo(1) == "optimizer")
    }
  }

  "parser getMetaLineFileInfo" should {
    "be able to pare '; (main:          |1-3)'" in {
      val Parsed.Success(result, _) =
        parse("; main:1-3", getHeaderBlockMetaLineFileInfo(_))
      assert(result.fileName == "main")
      assert(result.lineStart == 1)
      assert(result.lineEnd == 3)
    }
  }

  "parseHeaderBlockMetaLineFileInfo" should {
    "be able to handle a path with a colon" in {
      val line = "; /home/simon/tmp/bytecode-cpg/trivial:try-catch.php:1-23"
      val Parsed.Success(result,length) = parse(line,getHeaderBlockMetaLineFileInfo(_))
      length shouldBe line.length
      result.fileName shouldBe "/home/simon/tmp/bytecode-cpg/trivial:try-catch.php"
    }
    "be able to handle path without colon" in {
      val line = "   ; /tmp/php2cpg17511137408163283203/test.php:1-9"
      val Parsed.Success(result,length) = parse(line,getHeaderBlockMetaLineFileInfo(_))
      length shouldBe line.length
      result.fileName shouldBe "/tmp/php2cpg17511137408163283203/test.php"
      result.lineStart shouldBe 1
      result.lineEnd shouldBe 9
    }
  }

  "parser parseHeaderBlock" should {
    "be able to parse a valid header block with colon in path" in {
      val headerBlock =
        """$_main:
          ; (lines=23, args=0, vars=2, tmps=8)
          ; (before optimizer)
          ; /home/simon/tmp/bytecode-cpg/trivial:try-catch.php:1-23
          ; return  [] RANGE[0..0]
""".stripMargin
      val Parsed.Success(result, length) = parse(headerBlock, getHeaderBlock(_))
      length shouldBe headerBlock.length
      result._3.fileName shouldBe "/home/simon/tmp/bytecode-cpg/trivial:try-catch.php"
      result._3.lineStart shouldBe 1
      result._3.lineEnd shouldBe 23
    }
    "be able to parse a valid header block" in {
      val headerBlock =
        """$_main:
  ; (lines=23, args=0, vars=2, tmps=8)
  ; (before optimizer)
  ; /home/simon/tmp/bytecode-cpg/trivial-try-catch.php:1-23
  ; return  [] RANGE[0..0]
"""
      val Parsed.Success(result, length) = parse(headerBlock, getHeaderBlock(_))
      assert(length == headerBlock.length)
      assert(result._1.name == "dlr_main")
      assert(result._1.lines == 23)
      assert(result._1.args == 0)
      assert(result._1.vars == 2)
      assert(result._1.tmps == 8)
      assert(result._2.metaInfo.length == 2)
      assert(result._2.metaInfo.head == "before")
      assert(result._2.metaInfo(1) == "optimizer")
      assert(
        result._3.fileName == "/home/simon/tmp/bytecode-cpg/trivial-try-catch.php")
      assert(result._3.lineStart == 1)
      assert(result._3.lineEnd == 23)
      result._4.string shouldBe "return  [] RANGE[0..0]"
    }
    "be able to parse a valid header block with class definition" in {
      val headerBlock =
        """Basic::__init__:
  ; (lines=23, args=0, vars=2, tmps=8)
  ; (before optimizer)
  ; /home/simon/tmp/bytecode-cpg/trivial-try-catch.php:1-23
  ; return  [] RANGE[0..0]
"""
      val Parsed.Success(result, length) = parse(headerBlock, getHeaderBlock(_))
      length shouldBe headerBlock.length
      result._1.name shouldBe "__init__"
      result._1.classname shouldBe Some("basic")
    }
    "be able to parse a valid header block with namespace and class" in {
      val headerBlock =
        """namespace\Basic::__init__:
  ; (lines=23, args=0, vars=2, tmps=8)
  ; (before optimizer)
  ; /home/simon/tmp/bytecode-cpg/trivial-try-catch.php:1-23
  ; return  [] RANGE[0..0]
"""
      val Parsed.Success(result, length) = parse(headerBlock, getHeaderBlock(_))
      length shouldBe headerBlock.length
      result._1.name shouldBe "__init__"
      result._1.classname shouldBe Some("basic")
      result._1.namespace shouldBe Some("namespace")
    }
    "be able to parse a valid header block with namespace" in {
      val headerBlock =
        """namespace\__init__:
  ; (lines=23, args=0, vars=2, tmps=8)
  ; (before optimizer)
  ; /home/simon/tmp/bytecode-cpg/trivial-try-catch.php:1-23
  ; return  [] RANGE[0..0]
"""
      val Parsed.Success(result, length) = parse(headerBlock, getHeaderBlock(_))
      length shouldBe headerBlock.length
      result._1.name shouldBe "__init__"
      result._1.classname shouldBe None
      result._1.namespace shouldBe Some("namespace")
    }
    "be able to parse a valid header block with deep namespace" in {
      val headerBlock =
        """start\namespace\__init__:
  ; (lines=23, args=0, vars=2, tmps=8)
  ; (before optimizer)
  ; /home/simon/tmp/bytecode-cpg/trivial-try-catch.php:1-23
  ; return  [] RANGE[0..0]
"""
      val Parsed.Success(result, length) = parse(headerBlock, getHeaderBlock(_))
      length shouldBe headerBlock.length
      result._1.name shouldBe "__init__"
      result._1.classname shouldBe None
      result._1.namespace shouldBe Some("start\\namespace")
    }
    "be able to parse a valid head block with spaces in file name" in {
      val headerBlock =
        """$_main:
          |  ; (lines=3, args=0, vars=0, tmps=0)
          |  ; (before optimizer)
          |  ; /home/simon/tmp/cpgIssues/PoC/People Weird(@#.php:1-3
          |  ; return  [] RANGE[0..0]
          |""".stripMargin
      val Parsed.Success(result, length) = parse(headerBlock, getHeaderBlock(_))
      length shouldBe headerBlock.length
      result._1.name shouldBe "dlr_main"
      result._3.fileName shouldBe "/home/simon/tmp/cpgIssues/PoC/People Weird(@#.php"
      result._3.lineStart shouldBe 1
      result._3.lineEnd shouldBe 3
    }
  }

}
