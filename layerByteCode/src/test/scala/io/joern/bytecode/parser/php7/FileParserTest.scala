package io.joern.bytecode.parser.php7

import fastparse._
import io.joern.bytecode.parser.constructs.MethodDefinitionPair
import io.joern.bytecode.parser.php7.EasyBase64.encode
import io.joern.config.CPGConfig
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.io.File
import scala.sys.process._

class FileParserTest extends AnyWordSpec with Matchers {

  val config: CPGConfig = CPGConfig.initializeConfig()

  "parser parseLastLine" should {
    "be able to consume 'No syntax errors detected in garbage'" in {
      val Parsed.Success(_, count) =
        parse("No syntax errors detected in garbage",
              FileParser7.parseLastLine(_))
      assert(count == "No syntax errors detected in garbage".length)
    }
  }

  "actualParse" should {
    "detect a PHP Parser Error" in {
      val fullDump : String = "PHP Parse error: syntax error, unexpected new T_NEW"
      an [FileParser7.PHPParseError] should be thrownBy FileParser7.actualParse(fullDump, "file")
    }
    "detect a PHP Fatal Error" in {
      val fullDump : String = "PHP Fatal error:  Cannot declare class MyClass,"
      an [FileParser7.PHPFatalError] should be thrownBy FileParser7.actualParse(fullDump, "file")
    }
    "detect an internal error" in {
      val fullDump : String = "$_main: ; (lines=6, args=0, vars=1, tmps=3)"
      an [FileParser7.BytecodeParseError] should be thrownBy FileParser7.actualParse(fullDump, "file")
    }
  }

  "parser parseByteCodeDump" should {
    "be able to parse single function single BB" in {
      val fullDump =
        s"""
$$_main: ; (lines=6, args=0, vars=1, tmps=3)
        ; (before optimizer)
        ; trivial-main.php:1-4
L0 (2):     INIT_FCALL 1 96 string("${encode("phpinfo")}")
L1 (2):     T1 = CONCAT string("${encode("conca")}") CV0($$var)
L2 (2):     T2 = CONCAT T1 string("${encode("tenation")}")
L3 (2):     SEND_VAL T2 1
L4 (2):     DO_ICALL
L5 (4):     RETURN int(1)

$$_main: ; (lines=6, args=0, vars=1, tmps=3)
        ; (before block pass)
        ; trivial-main.php:1-4
BB0: start exit lines=[0-5]
     INIT_FCALL 1 96 string("${encode("phpinfo")}")
     T1 = CONCAT string("${encode("conca")}") CV0($$var)
     T2 = CONCAT T1 string("${encode("tenation")}")
     SEND_VAL T2 1
     DO_ICALL
     RETURN int(1)
"""
      val Parsed.Success(results, count) =
        parse(fullDump, FileParser7.parseByteCodeDump(_))
      assert(results.length == 1)
      val MethodDefinitionPair(byteCode, controlFlow) = results.head
      val result = (byteCode, controlFlow)
      assert(count == fullDump.length)
      assert(result._1.instructions.length == 6)
      assert(result._2.blocks.length == 1)
      assert(result._2.blocks.head.instructions.length == 6)
    }
  }

  "ByteCodeParser.parse" should {
    "be able to directly deal with a php file" in {
      val cwd = "pwd".!!.stripMargin.trim
      val methodDefinitionPairs =
        FileParser7.parseFromFile(new File(
          cwd + "/layerByteCode/resources/unittesting/testprojects/onlyMainCreation/trivial-php.php"), config.php7.interpreter, config.php7.phpini)
      assert(methodDefinitionPairs.length == 1)
      assert(methodDefinitionPairs.head.byteCodeBlock.instructions.length == 6)
    }
    "be able to extract multiple methods from the same file if they exist" in {
      val cwd = "pwd".!!.stripMargin.trim
      val multipleMethodsPairs =
        FileParser7.parseFromFile(new File(
          cwd + "/layerByteCode/resources/unittesting/testprojects/twoFunctionsAndMain/main.php"), config.php7.interpreter, config.php7.phpini)
      assert(multipleMethodsPairs.length == 3)
    }
    "be able to extract multiple BB in basicConditional project" in {
      val cwd = "pwd".!!.stripMargin.trim
      val multipleMethodPairs = FileParser7.parseFromFile(new File(
        cwd + "/layerByteCode/resources/unittesting/testprojects/basicConditional/main.php"), config.php7.interpreter, config.php7.phpini)
      multipleMethodPairs.length shouldBe 1
      multipleMethodPairs.head.controlFlowBlock.blocks.length shouldBe 4
    }
    "be ebale to extract multiple defintion pairs in project" in {
      val cwd = "pwd".!!.stripMargin.trim
      val multipleMethodPairs = FileParser7.parseFromFile(new File(
        cwd + "/layerByteCode/resources/unittesting/testprojects/singleClassProject/main.php"), config.php7.interpreter, config.php7.phpini)
      multipleMethodPairs.length shouldBe 4
    }
    "be able to deal with unicode" when {
      "in identifier" in {
        val cwd = "pwd".!!.stripMargin.trim
        try {
          FileParser7.parseFromFile(new File(
            cwd + "/layerByteCode/resources/unittesting/testprojects/unicode/main.php"), config.php7.interpreter, config.php7.phpini)
        } catch {
          case x : Throwable =>
            fail(x.getMessage)
        }
      }
      "in function name" in {
        val cwd = "pwd".!!.stripMargin.trim
        try {
          val res = FileParser7.parseFromFile(new File(
            cwd + "/layerByteCode/resources/unittesting/testprojects/unicode/hiragana.php"), config.php7.interpreter, config.php7.phpini)
          res.map(_.byteCodeBlock.name).toSet shouldBe Set("dlr_main", "rさ")
        } catch {
          case x: Throwable =>
            fail(x.getMessage)
        }
      }
    }
  }
}
