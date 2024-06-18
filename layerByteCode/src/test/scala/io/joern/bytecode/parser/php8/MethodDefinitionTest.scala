package io.joern.bytecode.parser.php8

import fastparse._
import io.joern.bytecode.parser.EasyBase64.encode
import io.joern.bytecode.parser.php8.MethodDefinition.getFullMethodDefinitionBlock
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MethodDefinitionTest extends AnyWordSpec with Matchers {

  "parser getFullMethodDefintionBlock" should {
    "be able to parse a single MethodDefintionBlock fully" in {
      val block = s"""$$_main:
                    |     ; (lines=10, args=0, vars=1, tmps=4)
                    |     ; (before optimizer)
                    |     ; /home/malte/coding/uni/master/testproject/tests/new.php:1-4
                    |     ; return  [] RANGE[0..0]
                    |0000 EXT_STMT
                    |0001 V1 = NEW 1 string("${encode("Basic")}")
                    |0002 SEND_VAL_EX string("${encode("value")}") 1
                    |0003 DO_FCALL
                    |0004 ASSIGN CV0($$var) V1
                    |0005 EXT_STMT
                    |0006 INIT_METHOD_CALL 1 CV0($$var) string("${encode("test2")}")
                    |0007 SEND_VAL_EX string("${encode("Basic")}") 1
                    |0008 DO_FCALL
                    |0009 RETURN int(1)
                    |LIVE RANGES:
                    |     1: 0002 - 0004 (new)
                    |
                    |$$_main:
                    |     ; (lines=10, args=0, vars=1, tmps=4)
                    |     ; (before block pass)
                    |     ; /home/malte/coding/uni/master/testproject/tests/new.php:1-4
                    |     ; return  [] RANGE[0..0]
                    |BB0:
                    |     ; start exit lines=[0-9]
                    |0000 EXT_STMT
                    |0001 V1 = NEW 1 string("${encode("Basic")}")
                    |0002 SEND_VAL_EX string("${encode("value")}") 1
                    |0003 DO_FCALL
                    |0004 ASSIGN CV0($$var) V1
                    |0005 EXT_STMT
                    |0006 INIT_METHOD_CALL 1 CV0($$var) string("${encode("test2")}")
                    |0007 SEND_VAL_EX string("${encode("other")}") 1
                    |0008 DO_FCALL
                    |0009 RETURN int(1)
                    |""".stripMargin
      val Parsed.Success(_, length) =
        parse(block, getFullMethodDefinitionBlock(_))
      length shouldBe block.length
    }
  }

}

