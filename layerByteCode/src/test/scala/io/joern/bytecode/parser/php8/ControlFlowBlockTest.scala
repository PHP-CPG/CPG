package io.joern.bytecode.parser.php8

import fastparse._
import io.joern.bytecode.parser.EasyBase64.encode
import io.joern.bytecode.parser.constructs._
import io.joern.bytecode.parser.php8.ControlFlowBlock._
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
      val line =
        """BB32:
          |  ; follow exit finally_end lines=[272-272]""".stripMargin
      val Parsed.Success(result, length) = parse(line, getBBDefinitionLine(_))
      length shouldBe line.length
      result shouldBe BBDefinitionLine(32,Seq("follow","exit","finally_end"),272,272)
    }
    "be able to parse BB0 start exit lines=[0-5]" in {
      val Parsed.Success(result, _) =
        parse("BB0:\n     ; start exit lines=[0-5]", getBBDefinitionLine(_))
      assert(result.number == 0)
      assert(result.attributes.length == 2)
      assert(result.attributes.head == "start")
      assert(result.attributes(1) == "exit")
      assert(result.firstInstruction == 0)
      assert(result.lastInstruction == 5)
    }
    "be able to parse BB4 catch lines=[17-17]" in {
      val line = """BB4:
                   |     ; catch lines=[17-17]""".stripMargin
      val Parsed.Success(result, length) =
        parse(line, getBBDefinitionLine(_))
      length shouldBe line.length
      result.attributes shouldBe List("catch")
      result.firstInstruction shouldBe 17
      result.lastInstruction shouldBe 17
    }
    "be able to parse BB2: unreachable unreachable_free lines=[12-13]" in {
      val line =
        """BB2:
          |  ; unreachable unreachable_free lines=[12-13]""".stripMargin
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
        """BB0:
     ; start lines=[0-2]
     ; to=(BB2, BB1)
0001 ASSIGN CV0($x) int(42)
0002 T2 = IS_EQUAL CV0($x) int(43)
0003 JMPZ T2 BB2
"""
      val nextBBStart = """BB1:
                          |     ; follow lines[3-4]""".stripMargin
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
        s"""BB4:
          |  ; catch lines=[17-17]
          |  ; to=(BB5)
          |0001 CV1($$e) = CATCH string("${encode("Exception")}")
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
        """$_main:
          |     ; (lines=42, args=3, vars=3, tmps=2)
          |     ; (before block pass)
          |     ; main:23-42
          |     ; return  [] RANGE[0..0]
          |BB0:
          |     ; start exit lines=[0-1]
          |0000 CONCAT T1 T2
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
        """$_main:
          |     ; (lines=42, args=3, vars=3, tmps=2)
          |     ; (before block pass)
          |     ; main:23-42
          |     ; return  [] RANGE[0..0]
          |BB0:
          |     ; start exit lines=[0-1]
          |0000 CONCAT T1 T2
          |0001 DO_ICALL
          |""".stripMargin
      val Parsed.Success(result, count) =
        parse(singleLineByteCodeBlock, getControlFlowBlock(_))
      assert(count == singleLineByteCodeBlock.length)
      assert(result.blocks.length == 1)
      assert(result.blocks.head.instructions.length == 2)
    }
    "be able to parse #247" in {
      val dump = """$_main:
                   |     ; (lines=9, args=0, vars=0, tmps=3)
                   |     ; (before block pass)
                   |     ; ../testproject/tmp.php:1-3
                   |     ; return  [] RANGE[0..0]
                   |BB0:
                   |     ; start lines=[0-3]
                   |     ; to=(BB2, BB1)
                   |0000 NOP
                   |0001 NOP
                   |0002 NOP
                   |0003 T1 = JMPZ_EX bool(false) BB2
                   |
                   |BB1:
                   |     ; follow lines=[4-7]
                   |     ; to=(BB2)
                   |0004 INIT_FCALL 1 96 string("aW5pX2dldA==")
                   |0005 SEND_VAL string("bWJzdHJpbmcuZnVuY19vdmVybG9hZA==") 1
                   |0006 V2 = DO_ICALL
                   |0007 T1 = BOOL V2
                   |
                   |BB2:
                   |     ; follow target exit lines=[8-8]
                   |0008 RETURN int(1)
                   |""".stripMargin
      val Parsed.Success(result, count) = parse(dump, getControlFlowBlock(_))
      count shouldBe dump.length
      result.blocks.length shouldBe 3
      result.blocks.head.instructions.length shouldBe 4
    }
    "be able to parse multiple BB" in {
      val singleLineByteCodeBlock =
        """$_main:
          |     ; (lines=42, args=3, vars=3, tmps=2)
          |     ; (before block pass)
          |     ; main:23-42
          |     ; return  [] RANGE[0..0]
          |BB0:
          |     ; start exit lines=[0-1]
          |     ; to=(BB1)
          |0000 CONCAT T1 T2
          |0001 DO_ICALL
          |
          |BB1:
          |     ; exit lines=[0-2]
          |0002 CONCAT T1 T2
          |0002 DO_ICALL
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
        s"""$$_main:
          |     ; (lines=7, args=0, vars=1, tmps=3)
          |     ; (before block pass)
          |     ; trivial-main.php:1-4
          |     ; return  [] RANGE[0..0]
          |BB0:
          |     ; start exit lines=[0-6]
          |0000 EXT_STMT
          |0001 INIT_FCALL 1 96 string("${encode("phpinfo")}")
          |0002 T1 = CONCAT string("${encode("conca")}") CV0($$var)
          |0003 T2 = CONCAT T1 string("${encode("tentation")}")
          |0004 SEND_VAL T2 1
          |0005 DO_FCALL
          |0006 RETURN int(1)
          |""".stripMargin
      val Parsed.Success(result, count) =
        parse(fullDump, getControlFlowBlock(_))
      assert(count == fullDump.length)
      assert(result.blocks.length == 1)
    }
    "be able to parse proper long (partial) dump" in {
      val fullDump =
        s"""PleskXTest\\TestCase::tearDownAfterClass:
          |     ; (lines=21, args=0, vars=2, tmps=6)
          |     ; (before block pass)
          |     ; /home/malte/coding/uni/master/testproject/tests/plestc.php:32-40
          |     ; return  [] RANGE[0..0]
          |BB0:
          |     ; start lines=[0-2]
          |     ; to=(BB6, BB1)
          |0000 EXT_STMT
          |0001 T2 = FETCH_STATIC_PROP_R string("${encode("webspaces")}") (self) (exception)
          |0002 V3 = FE_RESET_R T2 BB6
          |
          |BB1:
          |     ; follow target lines=[3-3]
          |     ; to=(BB6, BB2)
          |0003 FE_FETCH_R V3 CV0($$webspace) BB6
          |
          |BB2:
          |     ; follow lines=[4-4]
          |     ; to=(BB3)
          |0004 NOP
          |
          |BB3:
          |     ; follow try lines=[5-15]
          |     ; to=(BB1)
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
          |0015 JMP BB1
          |
          |BB4:
          |     ; catch lines=[16-16]
          |     ; to=(BB5)
          |0016 CV1($$e) = CATCH string("${encode("Exception")}")
          |
          |BB5:
          |     ; follow lines=[17-17]
          |     ; to=(BB1)
          |0017 JMP BB1
          |
          |BB6:
          |     ; target exit lines=[18-20]
          |0018 FE_FREE V3
          |0019 EXT_STMT
          |0020 RETURN null
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
        s"""PleskXTest\\Utility\\KeyLimitChecker::checkByType:
          |     ; (lines=66, args=3, vars=4, tmps=17)
          |     ; (before block pass)
          |     ; KeyLimitChecker.php:21-45
          |     ; return  [] RANGE[0..0]
          |BB0:
          |     ; start lines=[0-6]
          |     ; to=(BB5, BB8, BB11, BB12, BB1)
          |0000 CV0($$keyInfo) = RECV 1
          |0001 CV1($$type) = RECV 2
          |0002 CV2($$minimalRequirement) = RECV 3
          |0003 EXT_STMT
          |0004 ASSIGN CV3($$field) null
          |0005 EXT_STMT
          |0006 SWITCH_STRING CV1($$type) "${encode("limit_clients")}": BB5, "${encode("limit_resellers")}": BB8, "${encode("limit_domains")}": BB11, default: BB12
          |
          |BB1:
          |     ; follow target lines=[7-8]
          |     ; to=(BB5, BB2)
          |0007 T5 = IS_EQUAL CV1($$type) string("${encode("limit_clients")}")
          |0008 JMPNZ T5 BB5
          |
          |BB2:
          |     ; follow lines=[9-10]
          |     ; to=(BB8, BB3)
          |0009 T5 = IS_EQUAL CV1($$type) string("${encode("limit_resellers")}")
          |0010 JMPNZ T5 BB8
          |
          |BB3:
          |     ; follow lines=[11-12]
          |     ; to=(BB12, BB11)
          |0011 T5 = IS_EQUAL CV1($$type) string("${encode("limit_domains")}")
          |0012 JMPZNZ T5 BB12 BB11
          |
          |BB4:
          |     ; unreachable lines=[13-13]
          |     ; to=(BB12)
          |0013 JMP BB12
          |""".stripMargin
      val Parsed.Success(result, length) = parse(dump, getControlFlowBlock(_))
      length shouldBe dump.length
      result.blocks.length shouldBe 5
    }
  }

}
