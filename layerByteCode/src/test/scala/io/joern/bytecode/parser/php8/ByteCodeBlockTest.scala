package io.joern.bytecode.parser.php8

import fastparse._
import io.joern.bytecode.parser.EasyBase64.encode
import io.joern.bytecode.parser.constructs._
import io.joern.bytecode.parser.php8.ByteCodeBlock._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ByteCodeBlockTest extends AnyWordSpec with Matchers {

  /*"parser getLineNumber" should {
    "be able to parse L42" in {
      val Parsed.Success(result, _) = parse(input = "L42", getLineNumber(_))
      assert(result == 42)
    }
  }*/

  /*"parser getFileLineNumber" should {
    "be able to parse (43)" in {
      val Parsed.Success(result, _) =
        parse(input = "(43)", getFileLineNUmber(_))
      assert(result == 43)
    }
  }*/

  "parser getDefiningInstructionLine" should {
    "be able to parse 0042 DO_ICALL" in {
      val Parsed.Success(result, count) =
        parse(input = "0042 DO_ICALL", getDefiningInstructionLine(_))
      assert(count == 13)
      assert(result.fileLine.isEmpty)
      assert(result.opNumber.contains(42))
      result.instruction match {
        case instruction: Operation =>
          instruction.op match {
            case NoValueOperation("DO_ICALL") => //passing the test
            case _                            => fail(message = "the operation is not a NoValueOperation")
          }
        case _ => fail()
      }
    }
    "be able to parse 0000 V1 = NEW 0 string(\"Basic\")" in {
      val line = s"""0000 V1 = NEW 0 string("${encode("Basic")}")"""
      val Parsed.Success(result, count) =
        parse(line, getDefiningInstructionLine(_))
      count shouldBe line.length
      result.opNumber shouldBe Some(0)
      // result.fileLine shouldBe Some(20)
    }
  }

  "parser getMethodBlockByteCode" should {
    "be able to parse single instruction code block" in {
      val singleLineByteCodeBlock =
        """$_main:
          |     ; (lines=42, args=3, vars=3, tmps=2)
          |     ; (before optimizer)
          |     ; main:23-42
          |     ; return  [] RANGE[0..0]
          |0000 CONCAT T1 T2
          |""".stripMargin
      val Parsed.Success(result, count) =
        parse(singleLineByteCodeBlock, getByteCodeBlock(_))
      assert(count == singleLineByteCodeBlock.length)
      assert(result.name == "dlr_main")
      assert(result.lines == 42)
      assert(result.args == 3)
      assert(result.vars == 3)
      assert(result.tmps == 2)
      assert(result.parsingMetaInfo.head == "before")
      assert(result.parsingMetaInfo(1) == "optimizer")
      assert(result.fileName == "main")
      assert(result.lineStart == 23)
      assert(result.lineEnd == 42)
      assert(result.instructions.length == 1)
      result.instructions.head match {
        case instr: InstructionLine =>
          assert(instr.opNumber.contains(0))
          // assert(instr.fileLine == Some(23))
          instr.instruction match {
            case instr: Operation =>
              instr.op match {
                case DualValueOperation("CONCAT", _, _) =>
                case _                                  => fail()
              }
            case _ => fail()
          }
        case _ => fail()
      }
    }
    "be able to parse multi instructions code block" in {
      val singleLineByteCodeBlock =
        """$_main:
          |     ; (lines=42, args=3, vars=3, tmps=2)
          |     ; (before optimizer)
          |     ; main:23-42
          |     ; return  [] RANGE[0..0]
          |0000 CONCAT T1 T2
          |0001 T3 = CONCAT T1 T2
          |""".stripMargin
      val Parsed.Success(result, count) =
        parse(singleLineByteCodeBlock, getByteCodeBlock(_))
      assert(count == singleLineByteCodeBlock.length)
      assert(result.name == "dlr_main")
      assert(result.lines == 42)
      assert(result.args == 3)
      assert(result.vars == 3)
      assert(result.tmps == 2)
      assert(result.parsingMetaInfo.head == "before")
      assert(result.parsingMetaInfo(1) == "optimizer")
      assert(result.fileName == "main")
      assert(result.lineStart == 23)
      assert(result.lineEnd == 42)
      assert(result.instructions.length == 2)
      result.instructions.head match {
        case instr: InstructionLine =>
          assert(instr.opNumber.contains(0))
          //assert(instr.fileLine.contains(23))
          instr.instruction match {
            case instr: Operation =>
              instr.op match {
                case DualValueOperation("CONCAT", _, _) =>
                case _                                  => fail()
              }
            case _ => fail()
          }
        case _ => fail()
      }
      result.instructions(1) match {
        case instr: InstructionLine =>
          assert(instr.opNumber.contains(1))
          //assert(instr.fileLine.contains(24))
          instr.instruction match {
            case _ : Assignment =>
            case _                 => fail()
          }
        case _ => fail()
      }
    }
    // ${encode("Basic")}
    "be able to parse the beginning of a proper dump" in {
      val fullDump =
        s"""$$_main:
          |     ; (lines=7, args=0, vars=1, tmps=3)
          |     ; (before optimizer)
          |     ; trivial-main.php:1-4
          |     ; return  [] RANGE[0..0]
          |0000 EXT_STMT
          |0001 INIT_FCALL 1 96 string("${encode("phpinfo")}")
          |0002 T1 = CONCAT string("${encode("conca")}") CV0($$var)
          |0003 T2 = CONCAT T1 string("${encode("tenation")}")
          |0004 SEND_VAL T2 1
          |0005 DO_FCALL
          |0006 RETURN int(1)
          |
          |$$_main:
          |     ; (lines=7, args=0, vars=1, tmps=3)
          |     ; (before block pass)
          |     ; trivial-main.php:1-4
          |     ; return  [] RANGE[0..0]
          |BB0:
          |     ; start exit lines=[0-6]
          |0000 EXT_STMT
          |0001 INIT_FCALL 1 96 string("${encode("phpinfo")}")
          |0002 T1 = CONCAT string("${encode("conca")}") CV0($$var)
          |0003 T2 = CONCAT T1 string("${encode("tenation")}")
          |0004 SEND_VAL T2 1
          |0005 DO_FCALL
          |0006 RETURN int(1)
          |""".stripMargin
      val Parsed.Success(_, count) = parse(fullDump, getByteCodeBlock(_))
      assert(count > 10) //not the best test but I was to lazy to count
    }
  }

  "be able to parse a dump containing a live range" in {
    val dump =
      s"""$$_main:
        |     ; (lines=10, args=0, vars=1, tmps=4)
        |     ; (before optimizer)
        |     ; /home/malte/coding/uni/master/testproject/tests/new.php:1-4
        |     ; return  [] RANGE[0..0]
        |0000 EXT_STMT
        |0001 V1 = NEW 1 string("${encode("Basic")}")
        |0002 SEND_VAL_EX string("${encode("value")}") 1
        |0003 DO_FCALL
        |0004 ASSIGN CV0($$ar) V1
        |0005 EXT_STMT
        |0006 INIT_METHOD_CALL 1 CV0($$var) string("${encode("test2")}")
        |0007 SEND_VAL_EX string("${encode("other")}") 1
        |0008 DO_FCALL
        |0009 RETURN int(1)
        |LIVE RANGES:
        |     1: 0002 - 0004 (new)
        |""".stripMargin
    val Parsed.Success(_, count) = parse(dump, getByteCodeBlock(_))
    count shouldBe dump.length
  }
  "be able to parse a dump containing live ranges and exception table" in {
    // file from https://github.com/plesk/api-php-lib/blob/47f5f2e7b03bd088f35c84c6c10187bcc4698bcc/tests/TestCase.php
    val dump =
      s"""PleskXTest\\TestCase::tearDownAfterClass:
        |     ; (lines=21, args=0, vars=2, tmps=6)
        |     ; (before optimizer)
        |     ; pleskTC.php:32-40
        |     ; return  [] RANGE[0..0]
        |0000 EXT_STMT
        |0001 T2 = FETCH_STATIC_PROP_R string("${encode("webspaces")}") (self) (exception)
        |0002 V3 = FE_RESET_R T2 0018
        |0003 FE_FETCH_R V3 CV0($$webspace) 0018
        |0004 NOP
        |0005 EXT_STMT
        |0006 T4 = FETCH_STATIC_PROP_R string("${encode("_client")}") (static) (exception)
        |0007 INIT_METHOD_CALL 0 T4 string("${encode("webspace")}")
        |0008 V5 = DO_FCALL
        |0009 INIT_METHOD_CALL 2 V5 string("${encode("delete")}")
        |0010 SEND_VAL_EX string("${encode("id")}") 1
        |0011 CHECK_FUNC_ARG 2
        |0012 V6 = FETCH_OBJ_FUNC_ARG (ref) CV0($$webspace) string("${encode("id")}")
        |0013 SEND_FUNC_ARG V6 2
        |0014 DO_FCALL
        |0015 JMP 0017
        |0016 CV1($$e) = CATCH string("${encode("Exception")}")
        |0017 JMP 0003
        |0018 FE_FREE V3
        |0019 EXT_STMT
        |0020 RETURN null
        |LIVE RANGES:
        |     3: 0003 - 0018 (loop)
        |EXCEPTION TABLE:
        |     0005, 0016, -, -
        |""".stripMargin
    val Parsed.Success(_, count) = parse(dump, getByteCodeBlock(_))
    count shouldBe dump.length
  }

}
