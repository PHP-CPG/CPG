package io.joern.bytecode.parser.php7

import fastparse._
import io.joern.bytecode.parser.constructs._
import io.joern.bytecode.parser.php7.ByteCodeBlock._
import io.joern.bytecode.parser.php7.EasyBase64.encode
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ByteCodeBlockTest extends AnyWordSpec with Matchers {

  "parser getLineNumber" should {
    "be able to parse L42" in {
      val Parsed.Success(result, _) = parse(input = "L42", getLineNumber(_))
      assert(result == 42)
    }
  }

  "parser getFileLineNumber" should {
    "be able to parse (43)" in {
      val Parsed.Success(result, _) =
        parse(input = "(43)", getFileLineNUmber(_))
      assert(result == 43)
    }
  }

  "parser getDefiningInstructionLine" should {
    "be able to parse L1 (42): DO_ICALL" in {
      val Parsed.Success(result, count) =
        parse(input = "L1 (42): DO_ICALL", getDefiningInstructionLine(_))
      assert(count == 17)
      assert(result.fileLine.contains(42))
      assert(result.opNumber.contains(1))
      result.instruction match {
        case instruction: Operation =>
          instruction.op match {
            case NoValueOperation("DO_ICALL") => //passing the test
            case _                            => fail(message = "the operation is not a NoValueOperation")
          }
        case _ => fail()
      }
    }
    "be able to parse L0 (20):    V1 = NEW 1 string(\"Basic\")" in {
      val line = s"""L0 (20):    V1 = NEW 1 string("${encode("Basic")}")"""
      val Parsed.Success(result, count) =
        parse(line, getDefiningInstructionLine(_))
      count shouldBe line.length
      result.opNumber shouldBe Some(0)
      result.fileLine shouldBe Some(20)
    }
  }

  "parser getMethodBlockByteCode" should {
    "be able to parse single instruction code block" in {
      val singleLineByteCodeBlock =
        """$_main: ; (lines=42, args=3, vars=3, tmps=2)
          |        ; (before optimizer)
          |        ; main:23-42
          |L0 (23):          CONCAT T1 T2
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
          assert(instr.fileLine.contains(23))
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
        """$_main: ; (lines=42, args=3, vars=3, tmps=2)
          |        ; (before optimizer)
          |        ; main:23-42
          |L0 (23):          CONCAT T1 T2
          |L1 (24):          T3 = CONCAT T1 T2
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
          assert(instr.fileLine.contains(23))
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
          assert(instr.fileLine.contains(24))
          instr.instruction match {
            case _ : Assignment =>
            case _                 => fail()
          }
        case _ => fail()
      }
    }
    "be able to parse the beginning of a proper dump" in {
      val fullDump =
        s"""$$_main: ; (lines=6, args=0, vars=1, tmps=3)
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
     RETURN int(1)""".stripMargin
      val Parsed.Success(_, count) = parse(fullDump, getByteCodeBlock(_))
      assert(count > 10) //not the best test but I was to lazy to count
    }
  }

  "be able to parse a dump containing a live range" in {
    val dump =
      s"""$$_main: ; (lines=8, args=0, vars=1, tmps=4)
        ; (before optimizer)
        ; /home/simon/tmp/bytecode-cpg/basicOOP.php:1-23
L0 (20):    V1 = NEW 1 string("${encode("Basic")}")
L1 (20):    SEND_VAL_EX string("${encode("value")}") 1
L2 (20):    DO_FCALL
L3 (20):    ASSIGN CV0($$var) V1
L4 (21):    INIT_METHOD_CALL 1 CV0($$var) string("${encode("test2")}")
L5 (21):    SEND_VAL_EX string("${encode("other")}") 1
L6 (21):    DO_FCALL
L7 (23):    RETURN int(1)
LIVE RANGES:
        1: L1 - L3 (new)
"""
    val Parsed.Success(_, count) = parse(dump, getByteCodeBlock(_))
    count shouldBe dump.length
  }
  "be able to parse a dump containing live ranges and exception table" in {
    val dump =
      s"""PleskXTest\\TestCase::tearDownAfterClass: ; (lines=22, args=0, vars=2, tmps=6)
        |    ; (before optimizer)
        |    ; /home/simon/tmp/api-php-lib/tests/TestCase.php:32-40
        |L0 (32):    EXT_NOP
        |L1 (34):    EXT_STMT
        |L2 (34):    T2 = FETCH_STATIC_PROP_R string("${encode("webspaces")}") (self) (exception)
        |L3 (34):    V3 = FE_RESET_R T2 L19
        |L4 (34):    FE_FETCH_R V3 CV0($$webspace) L19
        |L5 (35):    NOP
        |L6 (36):    EXT_STMT
        |L7 (36):    T4 = FETCH_STATIC_PROP_R string("${encode("_client")}") (static) (exception)
        |L8 (36):    INIT_METHOD_CALL 0 T4 string("${encode("webspace")}")
        |L9 (36):    V5 = DO_FCALL
        |L10 (36):   INIT_METHOD_CALL 2 V5 string("${encode("delete")}")
        |L11 (36):   SEND_VAL_EX string("${encode("id")}") 1
        |L12 (36):   CHECK_FUNC_ARG 2
        |L13 (36):   V6 = FETCH_OBJ_FUNC_ARG (ref) CV0($$webspace) string("${encode("id")}")
        |L14 (36):   SEND_FUNC_ARG V6 2
        |L15 (36):   DO_FCALL
        |L16 (36):   JMP L18
        |L17 (37):   CV1($$e) = CATCH string("${encode("Exception")}")
        |L18 (34):   JMP L4
        |L19 (34):   FE_FREE V3
        |L20 (40):   EXT_STMT
        |L21 (40):   RETURN null
        |LIVE RANGES:
        |        3: L4 - L19 (loop)
        |EXCEPTION TABLE:
        |        L6, L17, -, -
        |""".stripMargin
    val Parsed.Success(_, count) = parse(dump, getByteCodeBlock(_))
    count shouldBe dump.length
  }
}
