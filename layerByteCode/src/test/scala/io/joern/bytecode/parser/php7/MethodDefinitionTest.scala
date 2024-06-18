package io.joern.bytecode.parser.php7

import fastparse._
import io.joern.bytecode.parser.php7.EasyBase64.encode
import io.joern.bytecode.parser.php7.MethodDefinition.getFullMethodDefinitionBlock
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MethodDefinitionTest extends AnyWordSpec with Matchers {

  "parser getFullMethodDefintionBlock" should {
    "be able to parse a single MethodDefintionBlock fully" in {
      val block = s"""$$_main: ; (lines=8, args=0, vars=1, tmps=4)
                    |    ; (before optimizer)
                    |    ; /home/simon/tmp/bytecode-cpg/basicOOP.php:1-23
                    |L0 (20):    V1 = NEW 1 string("${encode("Basic")}")
                    |L1 (20):    SEND_VAL_EX string("${encode("value")}") 1
                    |L2 (20):    DO_FCALL
                    |L3 (20):    ASSIGN CV0($$var) V1
                    |L4 (21):    INIT_METHOD_CALL 1 CV0($$var) string("${encode("test2")}")
                    |L5 (21):    SEND_VAL_EX string("${encode("other")}") 1
                    |L6 (21):    DO_FCALL
                    |L7 (23):    RETURN int(1)
                    |LIVE RANGES:
                    |        1: L1 - L3 (new)
                    |
                    |$$_main: ; (lines=8, args=0, vars=1, tmps=4)
                    |    ; (before block pass)
                    |    ; /home/simon/tmp/bytecode-cpg/basicOOP.php:1-23
                    |BB0: start exit lines=[0-7]
                    |            V1 = NEW 1 string("${encode("Basic")}")
                    |            SEND_VAL_EX string("${encode("value")}") 1
                    |            DO_FCALL
                    |            ASSIGN CV0($$var) V1
                    |            INIT_METHOD_CALL 1 CV0($$var) string("${encode("test2")}")
                    |            SEND_VAL_EX string("${encode("other")}") 1
                    |            DO_FCALL
                    |            RETURN int(1)
                    |""".stripMargin
      val Parsed.Success(_, length) =
        parse(block, getFullMethodDefinitionBlock(_))
      length shouldBe block.length
    }
  }

}
