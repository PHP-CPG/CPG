package io.joern.bytecode.parser.php7

import fastparse._
import io.joern.bytecode.parser.constructs._
import io.joern.bytecode.parser.php7.ControlFlowBlock._
import io.joern.bytecode.parser.php7.EasyBase64.encode
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ControlFlowBlockTest extends AnyWordSpec with Matchers {

  "parser getBBInstructionLine" should {
    "be able to parse        DO_ICALL" in {
      val Parsed.Success(result, count) =
        parse("        DO_ICALL", getBBInstructionLine(_))
      assert(count == 16)
      assert(result.fileLine.isEmpty)
      assert(result.opNumber.isEmpty)
      result.instruction match {
        case instruction: Operation =>
          instruction.op match {
            case NoValueOperation("DO_ICALL") =>
            case _                            => fail()
          }
        case _ => fail()
      }
    }
    "be able to parse JMPZ T2 BB2" in {
      val instr = "     JMPZ T2 BB2"
      val Parsed.Success(result, length) = parse(instr, getBBInstructionLine(_))
      length shouldBe instr.length
      val InstructionLine(_, _, instruction) = result
      instruction match {
        case Operation(op) =>
          op match {
            case DualValueOperation(command, condition, target) =>
              command shouldBe "JMPZ"
              condition match {
                case Variable(name, tmp, _) =>
                  name shouldBe "T2"
                  tmp shouldBe true
                case _ => fail()
              }
              target match {
                case IntegerLiteral(value) => value shouldBe 2
                case _                     => fail()
              }
            case _ => fail()
          }
        case _ => fail()
      }
    }
  }

  "parser getBBDefinitionLine" should {
    "be able to parse BB32: follow exit finally_end lines=[272-272]" in {
      val line = "BB32: follow exit finally_end lines=[272-272]"
      val Parsed.Success(result, length) = parse(line, getBBDefinitionLine(_))
      length shouldBe line.length
      result shouldBe BBDefinitionLine(32,Seq("follow","exit","finally_end"),272,272)
    }
    "be able to parse BB0: start exit lines=[0-5]" in {
      val Parsed.Success(result, _) =
        parse("BB0: start exit lines=[0-5]", getBBDefinitionLine(_))
      assert(result.number == 0)
      assert(result.attributes.length == 2)
      assert(result.attributes.head == "start")
      assert(result.attributes(1) == "exit")
      assert(result.firstInstruction == 0)
      assert(result.lastInstruction == 5)
    }
    "be able to parse BB4: catch lines=[17-17]" in {
      val line = "BB4: catch lines=[17-17]"
      val Parsed.Success(result, length) =
        parse(line, getBBDefinitionLine(_))
      length shouldBe line.length
      result.attributes shouldBe List("catch")
      result.firstInstruction shouldBe 17
      result.lastInstruction shouldBe 17
    }
    "be able to parse BB2: unreachable unreachable_free lines=[12-13]" in {
      val line = "BB2: unreachable unreachable_free lines=[12-13]"
      val Parsed.Success(result, length) = parse(line, getBBDefinitionLine(_))
      length shouldBe line.length
      result.attributes shouldBe List("unreachable", "unreachable_free")
      result.firstInstruction shouldBe 12
      result.lastInstruction shouldBe 13
    }
  }

  "parser getBBToLine" should {
    "be able to parse ; to=(BB0)" in {
      val Parsed.Success(result, _) = parse("  ; to=(BB0)", getBBToLine(_))
      assert(result.length == 1)
      assert(result.head == 0)
    }
    "be able to parse ; to=(BB0, BB1)" in {
      val Parsed.Success(result, _) = parse("  ; to=(BB0, BB1)", getBBToLine(_))
      assert(result.length == 2)
      assert(result.head == 0)
      assert(result(1) == 1)
    }
  }

  "parser getBasicBlock" should {
    "be able to parse a BasicBlock with JMPZ at the end but not including the next basic block" in {
      val bbstring =
        """BB0: start lines=[0-2]
          ; to=(BB2, BB1)
               ASSIGN CV0($x) int(42)
               T2 = IS_EQUAL CV0($x) int(43)
               JMPZ T2 BB2
"""
      val nextBBStart = "BB1: follow lines[3-4]"
      val fullTestString = bbstring + nextBBStart
      val Parsed.Success(result, length) =
        parse(fullTestString, getBasicBlock(_))
      length shouldBe bbstring.length
      result.firstInstruction shouldBe 0
      result.lastInstruction shouldBe 2
      result.number shouldBe 0
      result.followedBy.get.length shouldBe 2
      result.followedBy.get.head shouldBe 2
      result.followedBy.get(1) shouldBe 1
      result.instructions.length shouldBe 3
    }
    "be able to parse basic block with catch as its only keyword" in {
      val bb =
        s"""BB4: catch lines=[17-17]
          |    ; to=(BB5)
          |            CV1($$e) = CATCH string("${encode("Exception")}")
          |""".stripMargin
      val Parsed.Success(result, length) = parse(bb, getBasicBlock(_))
      length shouldBe bb.length
      result.attributes shouldBe List("catch")
      result.followedBy shouldBe Some(List(5))
      result.firstInstruction shouldBe 17
      result.lastInstruction shouldBe 17
    }
  }

  "parser getMethodBlockControlFlow" should {
    "be able to parse single BB single instruction" in {
      val singleLineByteCodeBlock =
        """$_main: ; (lines=42, args=3, vars=3, tmps=2)
          |        ; (before block pass)
          |        ; main:23-42
          |BB0: start exit lines=[0-1]
          |           CONCAT T1 T2
          |""".stripMargin
      val Parsed.Success(result, count) =
        parse(singleLineByteCodeBlock, getControlFlowBlock(_))
      assert(count == singleLineByteCodeBlock.length)
      assert(result.parsingMetaInfo.length == 3)
      assert(result.parsingMetaInfo.head == "before")
      assert(result.parsingMetaInfo(1) == "block")
      assert(result.parsingMetaInfo(2) == "pass")
      assert(result.blocks.length == 1)
      assert(result.blocks.head.number == 0)
      assert(result.blocks.head.attributes.length == 2)
      assert(result.blocks.head.firstInstruction == 0)
      assert(result.blocks.head.lastInstruction == 1)
      assert(result.blocks.head.instructions.length == 1)
    }
    "be able to parse single BB multiple instructions" in {
      val singleLineByteCodeBlock =
        """$_main: ; (lines=42, args=3, vars=3, tmps=2)
          |        ; (before block pass)
          |        ; main:23-42
          |BB0: start exit lines=[0-1]
          |           CONCAT T1 T2
          |           DO_ICALL
          |""".stripMargin
      val Parsed.Success(result, count) =
        parse(singleLineByteCodeBlock, getControlFlowBlock(_))
      assert(count == singleLineByteCodeBlock.length)
      assert(result.blocks.length == 1)
      assert(result.blocks.head.instructions.length == 2)
    }
    "be able to parse #247" in {
      val dump = """$_main: ; (lines=9, args=0, vars=0, tmps=3)
                   |    ; (before block pass)
                   |    ; ../testproject/tmp.php:1-3
                   |BB0: start lines=[0-3]
                   |    ; to=(BB2, BB1)
                   |            NOP
                   |            NOP
                   |            NOP
                   |            T1 = JMPZ_EX bool(false) BB2
                   |BB1: follow lines=[4-7]
                   |    ; to=(BB2)
                   |            INIT_FCALL 1 96 string("aW5pX2dldA==")
                   |            SEND_VAL string("bWJzdHJpbmcuZnVuY19vdmVybG9hZA==") 1
                   |            V2 = DO_ICALL
                   |            T1 = BOOL V2
                   |BB2: follow target exit lines=[8-8]
                   |            RETURN int(1)
                   |""".stripMargin
      val Parsed.Success(result, count) = parse(dump, getControlFlowBlock(_))
      count shouldBe dump.length
      result.blocks.length shouldBe 3
      result.blocks.head.instructions.length shouldBe 4
    }
    "be able to parse multiple BB" in {
      val singleLineByteCodeBlock =
        """$_main: ; (lines=42, args=3, vars=3, tmps=2)
          |        ; (before block pass)
          |        ; main:23-42
          |BB0: start exit lines=[0-1]
          |    ; to=(BB1)
          |           CONCAT T1 T2
          |           DO_ICALL
          |BB1: exit lines=[0-2]
          |    CONCAT T1 T2
          |    DO_ICALL
          |""".stripMargin
      val Parsed.Success(result, count) =
        parse(singleLineByteCodeBlock, getControlFlowBlock(_))
      assert(count == singleLineByteCodeBlock.length)
      assert(result.blocks.length == 2)
      assert(result.blocks.head.followedBy.get.length == 1)
      assert(result.blocks.head.followedBy.get.head == 1)
      assert(result.blocks(1).followedBy.isEmpty)
    }
    "be able to parse proper (partial) dump" in {
      val fullDump =
        s"""$$_main: ; (lines=6, args=0, vars=1, tmps=3)
        ; (before block pass)
        ; trivial-main.php:1-4
BB0: start exit lines=[0-5]
     INIT_FCALL 1 96 string("${encode("phpinfo")}")
     T1 = CONCAT string("${encode("conca")}") CV0($$var)
     T2 = CONCAT T1 string("${encode("tenation")}")
     SEND_VAL T2 1
     DO_ICALL
     RETURN int(1)
""".stripMargin
      val Parsed.Success(result, count) =
        parse(fullDump, getControlFlowBlock(_))
      assert(count == fullDump.length)
      assert(result.blocks.length == 1)
    }
    "be able to parse proper long (partial) dump" in {
      val fullDump =
        s"""PleskXTest\\TestCase::tearDownAfterClass: ; (lines=22, args=0, vars=2, tmps=6)
          |    ; (before block pass)
          |    ; /home/simon/tmp/api-php-lib/tests/TestCase.php:32-40
          |BB0: start lines=[0-3]
          |    ; to=(BB6, BB1)
          |            EXT_NOP
          |            EXT_STMT
          |            T2 = FETCH_STATIC_PROP_R string("${encode("webspaces")}") (self) (exception)
          |            V3 = FE_RESET_R T2 BB6
          |BB1: follow target lines=[4-4]
          |    ; to=(BB6, BB2)
          |            FE_FETCH_R V3 CV0($$webspace) BB6
          |BB2: follow lines=[5-5]
          |    ; to=(BB3)
          |            NOP
          |BB3: follow try lines=[6-16]
          |    ; to=(BB1)
          |            EXT_STMT
          |            T4 = FETCH_STATIC_PROP_R string("${encode("_client")}") (static) (exception)
          |            INIT_METHOD_CALL 0 T4 string("${encode("webspace")}")
          |            V5 = DO_FCALL
          |            INIT_METHOD_CALL 2 V5 string("${encode("delete")}")
          |            SEND_VAL_EX string("id") 1
          |            CHECK_FUNC_ARG 2
          |            V6 = FETCH_OBJ_FUNC_ARG (ref) CV0($$webspace) string("${encode("id")}")
          |            SEND_FUNC_ARG V6 2
          |            DO_FCALL
          |            JMP BB1
          |BB4: catch lines=[17-17]
          |    ; to=(BB5)
          |            CV1($$e) = CATCH string("${encode("Exception")}")
          |BB5: follow lines=[18-18]
          |    ; to=(BB1)
          |            JMP BB1
          |BB6: target exit lines=[19-21]
          |            FE_FREE V3
          |            EXT_STMT
          |            RETURN null
          |EXCEPTION TABLE:
          |        BB3, BB4, -, -
          |""".stripMargin
      val Parsed.Success(result, length) =
        parse(fullDump, getControlFlowBlock(_))
      length shouldBe fullDump.length
      result.blocks.length shouldBe 7
      result.exceptionTable match {
        case Some(_) =>
        case None    => fail(message = "there should be an exception table block")
      }
    }
    "be able to parse partial (longer) dump II" in {
      val dump =
        s"""PleskXTest\\Utility\\KeyLimitChecker::checkByType: ; (lines=69, args=3, vars=4, tmps=20)
          |    ; (before block pass)
          |    ; /home/simon/tmp/api-php-lib/tests/Utility/KeyLimitChecker.php:21-45
          |BB0: start lines=[0-9]
          |    ; to=(BB4, BB1)
          |            EXT_NOP
          |            CV0($$keyInfo) = RECV 1
          |            CV1($$type) = RECV 2
          |            CV2($$minimalRequirement) = RECV 3
          |            EXT_STMT
          |            ASSIGN CV3($$field) null
          |            EXT_STMT
          |            NOP
          |            T5 = IS_EQUAL CV1($$type) string("${encode("limit_clients")}")
          |            JMPNZ T5 BB4
          |BB1: follow lines=[10-12]
          |    ; to=(BB7, BB2)
          |            NOP
          |            T5 = IS_EQUAL CV1($$type) string("${encode("limit_resellers")}")
          |            JMPNZ T5 BB7
          |BB2: follow lines=[13-15]
          |    ; to=(BB11, BB10)
          |            NOP
          |            T5 = IS_EQUAL CV1($$type) string("${encode("limit_domains")}")
          |            JMPZNZ T5 BB11 BB10
          |BB3: unreachable lines=[16-16]
          |    ; to=(BB11)
          |            JMP BB11
          |""".stripMargin
      val Parsed.Success(result, length) = parse(dump, getControlFlowBlock(_))
      length shouldBe dump.length
      result.blocks.length shouldBe 4
    }
  }

}
