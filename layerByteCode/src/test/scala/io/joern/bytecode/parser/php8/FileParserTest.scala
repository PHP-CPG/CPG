package io.joern.bytecode.parser.php8

import fastparse._
import io.joern.bytecode.parser.EasyBase64.encode
import io.joern.bytecode.parser.constructs.MethodDefinitionPair
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
              FileParser8.parseLastLine(_))
      assert(count == "No syntax errors detected in garbage".length)
    }
  }

  "actualParse" should {
    "detect a PHP Parser Error" in {
      val fullDump : String = "PHP Parse error: syntax error, unexpected new T_NEW"
      an [FileParser8.PHPParseError] should be thrownBy FileParser8.actualParse(fullDump, "file")
    }
    "detect a PHP Fatal Error" in {
      val fullDump : String = "PHP Fatal error:  Cannot declare class MyClass,"
      an [FileParser8.PHPFatalError] should be thrownBy FileParser8.actualParse(fullDump, "file")
    }
    "detect an internal error" in {
      val fullDump : String = "$_main: ; (lines=6, args=0, vars=1, tmps=3)"
      an [FileParser8.BytecodeParseError] should be thrownBy FileParser8.actualParse(fullDump, "file")
    }
  }

  "parser parseByteCodeDump" should {
    "be able to parse single function single BB" in {
      val fullDump =
        s"""
          |$$_main:
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
      val Parsed.Success(results, count) =
        parse(fullDump, FileParser8.parseByteCodeDump(_))
      assert(results.length == 1)
      val MethodDefinitionPair(byteCode, controlFlow) = results.head
      val result = (byteCode, controlFlow)
      assert(count == fullDump.length)
      assert(result._1.instructions.length == 7)
      assert(result._2.blocks.length == 1)
      assert(result._2.blocks.head.instructions.length == 7)
    }
    "be able to parse BB block with missing newline before CFG" in {
      /**
       * generated from https://github.com/symfony/finder/blob/5.4/SplFileInfo.php
       */
      val fullDump = """
                       |$_main:
                       |     ; (lines=2, args=0, vars=0, tmps=0)
                       |     ; (before optimizer)
                       |     ; finder/SplFileInfo.php:1-89
                       |     ; return  [] RANGE[0..0]
                       |0000 DECLARE_CLASS_DELAYED string("c3ltZm9ueVxjb21wb25lbnRcZmluZGVyXHNwbGZpbGVpbmZv") string("c3BsZmlsZWluZm8=")
                       |0001 RETURN int(1)
                       |
                       |$_main:
                       |     ; (lines=2, args=0, vars=0, tmps=0)
                       |     ; (before block pass)
                       |     ; finder/SplFileInfo.php:1-89
                       |     ; return  [] RANGE[0..0]
                       |BB0:
                       |     ; start exit lines=[0-1]
                       |0000 DECLARE_CLASS_DELAYED string("c3ltZm9ueVxjb21wb25lbnRcZmluZGVyXHNwbGZpbGVpbmZv") string("c3BsZmlsZWluZm8=")
                       |0001 RETURN int(1)
                       |
                       |Symfony\Component\Finder\SplFileInfo::__construct:
                       |     ; (lines=11, args=3, vars=3, tmps=3)
                       |     ; (before optimizer)
                       |     ; finder/SplFileInfo.php:29-34
                       |     ; return  [] RANGE[0..0]
                       |0000 CV0($file) = RECV 1
                       |0001 CV1($relativePath) = RECV 2
                       |0002 CV2($relativePathname) = RECV 3
                       |0003 INIT_STATIC_METHOD_CALL 1 (parent) (exception) CONSTRUCTOR
                       |0004 SEND_VAR_EX CV0($file) 1
                       |0005 DO_FCALL
                       |0006 ASSIGN_OBJ THIS string("cmVsYXRpdmVQYXRo")
                       |0007 OP_DATA CV1($relativePath)
                       |0008 ASSIGN_OBJ THIS string("cmVsYXRpdmVQYXRobmFtZQ==")
                       |0009 OP_DATA CV2($relativePathname)
                       |0010 RETURN null
                       |
                       |Symfony\Component\Finder\SplFileInfo::__construct:
                       |     ; (lines=11, args=3, vars=3, tmps=3)
                       |     ; (before block pass)
                       |     ; finder/SplFileInfo.php:29-34
                       |     ; return  [] RANGE[0..0]
                       |BB0:
                       |     ; start exit lines=[0-10]
                       |0000 CV0($file) = RECV 1
                       |0001 CV1($relativePath) = RECV 2
                       |0002 CV2($relativePathname) = RECV 3
                       |0003 INIT_STATIC_METHOD_CALL 1 (parent) (exception) CONSTRUCTOR
                       |0004 SEND_VAR_EX CV0($file) 1
                       |0005 DO_FCALL
                       |0006 ASSIGN_OBJ THIS string("cmVsYXRpdmVQYXRo")
                       |0007 OP_DATA CV1($relativePath)
                       |0008 ASSIGN_OBJ THIS string("cmVsYXRpdmVQYXRobmFtZQ==")
                       |0009 OP_DATA CV2($relativePathname)
                       |0010 RETURN null
                       |
                       |Symfony\Component\Finder\SplFileInfo::getRelativePath:
                       |     ; (lines=3, args=0, vars=0, tmps=1)
                       |     ; (before optimizer)
                       |     ; finder/SplFileInfo.php:43-46
                       |     ; return  [] RANGE[0..0]
                       |0000 T0 = FETCH_OBJ_R THIS string("cmVsYXRpdmVQYXRo")
                       |0001 RETURN T0
                       |0002 RETURN null
                       |
                       |Symfony\Component\Finder\SplFileInfo::getRelativePath:
                       |     ; (lines=3, args=0, vars=0, tmps=1)
                       |     ; (before block pass)
                       |     ; finder/SplFileInfo.php:43-46
                       |     ; return  [] RANGE[0..0]
                       |BB0:
                       |     ; start exit lines=[0-1]
                       |0000 T0 = FETCH_OBJ_R THIS string("cmVsYXRpdmVQYXRo")
                       |0001 RETURN T0
                       |
                       |BB1:
                       |     ; unreachable lines=[2-2]
                       |0002 RETURN null
                       |
                       |Symfony\Component\Finder\SplFileInfo::getRelativePathname:
                       |     ; (lines=3, args=0, vars=0, tmps=1)
                       |     ; (before optimizer)
                       |     ; finder/SplFileInfo.php:55-58
                       |     ; return  [] RANGE[0..0]
                       |0000 T0 = FETCH_OBJ_R THIS string("cmVsYXRpdmVQYXRobmFtZQ==")
                       |0001 RETURN T0
                       |0002 RETURN null
                       |
                       |Symfony\Component\Finder\SplFileInfo::getRelativePathname:
                       |     ; (lines=3, args=0, vars=0, tmps=1)
                       |     ; (before block pass)
                       |     ; finder/SplFileInfo.php:55-58
                       |     ; return  [] RANGE[0..0]
                       |BB0:
                       |     ; start exit lines=[0-1]
                       |0000 T0 = FETCH_OBJ_R THIS string("cmVsYXRpdmVQYXRobmFtZQ==")
                       |0001 RETURN T0
                       |
                       |BB1:
                       |     ; unreachable lines=[2-2]
                       |0002 RETURN null
                       |
                       |Symfony\Component\Finder\SplFileInfo::getFilenameWithoutExtension:
                       |     ; (lines=11, args=0, vars=1, tmps=3)
                       |     ; (before optimizer)
                       |     ; finder/SplFileInfo.php:60-65
                       |     ; return  [] RANGE[0..0]
                       |0000 INIT_METHOD_CALL 0 THIS string("Z2V0RmlsZW5hbWU=")
                       |0001 V1 = DO_FCALL
                       |0002 ASSIGN CV0($filename) V1
                       |0003 INIT_NS_FCALL_BY_NAME 2 string("U3ltZm9ueVxDb21wb25lbnRcRmluZGVyXHBhdGhpbmZv")
                       |0004 SEND_VAR_EX CV0($filename) 1
                       |0005 SEND_VAL_EX int(8) 2
                       |0006 V3 = DO_FCALL_BY_NAME
                       |0007 VERIFY_RETURN_TYPE V3
                       |0008 RETURN V3
                       |0009 VERIFY_RETURN_TYPE
                       |0010 RETURN null
                       |LIVE RANGES:
                       |     3: 0007 - 0008 (tmp/var)
                       |
                       |Symfony\Component\Finder\SplFileInfo::getFilenameWithoutExtension:
                       |     ; (lines=11, args=0, vars=1, tmps=3)
                       |     ; (before block pass)
                       |     ; finder/SplFileInfo.php:60-65
                       |     ; return  [] RANGE[0..0]
                       |BB0:
                       |     ; start exit lines=[0-8]
                       |0000 INIT_METHOD_CALL 0 THIS string("Z2V0RmlsZW5hbWU=")
                       |0001 V1 = DO_FCALL
                       |0002 ASSIGN CV0($filename) V1
                       |0003 INIT_NS_FCALL_BY_NAME 2 string("U3ltZm9ueVxDb21wb25lbnRcRmluZGVyXHBhdGhpbmZv")
                       |0004 SEND_VAR_EX CV0($filename) 1
                       |0005 SEND_VAL_EX int(8) 2
                       |0006 V3 = DO_FCALL_BY_NAME
                       |0007 VERIFY_RETURN_TYPE V3
                       |0008 RETURN V3
                       |
                       |BB1:
                       |     ; unreachable lines=[9-10]
                       |0009 VERIFY_RETURN_TYPE
                       |0010 RETURN null
                       |
                       |Symfony\Component\Finder\SplFileInfo::getContents:
                       |     ; (lines=24, args=0, vars=2, tmps=10)
                       |     ; (before optimizer)
                       |     ; finder/SplFileInfo.php:74-87
                       |     ; return  [] RANGE[0..0]
                       |0000 INIT_NS_FCALL_BY_NAME 1 string("U3ltZm9ueVxDb21wb25lbnRcRmluZGVyXHNldF9lcnJvcl9oYW5kbGVy")
                       |0001 T2 = DECLARE_LAMBDA_FUNCTION 0
                       |0002 BIND_LEXICAL (ref) T2 CV0($error)
                       |0003 SEND_VAL_EX T2 1
                       |0004 DO_FCALL_BY_NAME
                       |0005 INIT_NS_FCALL_BY_NAME 1 string("U3ltZm9ueVxDb21wb25lbnRcRmluZGVyXGZpbGVfZ2V0X2NvbnRlbnRz")
                       |0006 INIT_METHOD_CALL 0 THIS string("Z2V0UGF0aG5hbWU=")
                       |0007 V5 = DO_FCALL
                       |0008 SEND_VAR_NO_REF_EX V5 1
                       |0009 V6 = DO_FCALL_BY_NAME
                       |0010 ASSIGN CV1($content) V6
                       |0011 T4 = FAST_CALL 0013
                       |0012 JMP 0016
                       |0013 INIT_NS_FCALL_BY_NAME 0 string("U3ltZm9ueVxDb21wb25lbnRcRmluZGVyXHJlc3RvcmVfZXJyb3JfaGFuZGxlcg==")
                       |0014 DO_FCALL_BY_NAME
                       |0015 FAST_RET T4
                       |0016 T9 = TYPE_CHECK (false) CV1($content)
                       |0017 JMPZ T9 0022
                       |0018 V10 = NEW 1 string("UnVudGltZUV4Y2VwdGlvbg==")
                       |0019 SEND_VAR_EX CV0($error) 1
                       |0020 DO_FCALL
                       |0021 THROW V10
                       |0022 RETURN CV1($content)
                       |0023 RETURN null
                       |LIVE RANGES:
                       |     2: 0002 - 0003 (tmp/var)
                       |     10: 0019 - 0021 (new)
                       |EXCEPTION TABLE:
                       |     0005, -, 0013, 0015
                       |Symfony\Component\Finder\SplFileInfo::getContents:
                       |     ; (lines=24, args=0, vars=2, tmps=10)
                       |     ; (before block pass)
                       |     ; finder/SplFileInfo.php:74-87
                       |     ; return  [] RANGE[0..0]
                       |BB0:
                       |     ; start lines=[0-4]
                       |     ; to=(BB1)
                       |0000 INIT_NS_FCALL_BY_NAME 1 string("U3ltZm9ueVxDb21wb25lbnRcRmluZGVyXHNldF9lcnJvcl9oYW5kbGVy")
                       |0001 T2 = DECLARE_LAMBDA_FUNCTION 0
                       |0002 BIND_LEXICAL (ref) T2 CV0($error)
                       |0003 SEND_VAL_EX T2 1
                       |0004 DO_FCALL_BY_NAME
                       |
                       |BB1:
                       |     ; follow try lines=[5-11]
                       |     ; to=(BB3, BB2)
                       |0005 INIT_NS_FCALL_BY_NAME 1 string("U3ltZm9ueVxDb21wb25lbnRcRmluZGVyXGZpbGVfZ2V0X2NvbnRlbnRz")
                       |0006 INIT_METHOD_CALL 0 THIS string("Z2V0UGF0aG5hbWU=")
                       |0007 V5 = DO_FCALL
                       |0008 SEND_VAR_NO_REF_EX V5 1
                       |0009 V6 = DO_FCALL_BY_NAME
                       |0010 ASSIGN CV1($content) V6
                       |0011 T4 = FAST_CALL BB3
                       |
                       |BB2:
                       |     ; follow lines=[12-12]
                       |     ; to=(BB5)
                       |0012 JMP BB5
                       |
                       |BB3:
                       |     ; target finally lines=[13-14]
                       |     ; to=(BB4)
                       |0013 INIT_NS_FCALL_BY_NAME 0 string("U3ltZm9ueVxDb21wb25lbnRcRmluZGVyXHJlc3RvcmVfZXJyb3JfaGFuZGxlcg==")
                       |0014 DO_FCALL_BY_NAME
                       |
                       |BB4:
                       |     ; follow exit finally_end lines=[15-15]
                       |0015 FAST_RET T4
                       |
                       |BB5:
                       |     ; target lines=[16-17]
                       |     ; to=(BB7, BB6)
                       |0016 T9 = TYPE_CHECK (false) CV1($content)
                       |0017 JMPZ T9 BB7
                       |
                       |BB6:
                       |     ; follow exit lines=[18-21]
                       |0018 V10 = NEW 1 string("UnVudGltZUV4Y2VwdGlvbg==")
                       |0019 SEND_VAR_EX CV0($error) 1
                       |0020 DO_FCALL
                       |0021 THROW V10
                       |
                       |BB7:
                       |     ; target exit lines=[22-22]
                       |0022 RETURN CV1($content)
                       |
                       |BB8:
                       |     ; unreachable lines=[23-23]
                       |0023 RETURN null
                       |EXCEPTION TABLE:
                       |        BB1, -, BB3, BB4
                       |
                       |Symfony\Component\Finder\{closure}:
                       |     ; (lines=5, args=2, vars=3, tmps=1)
                       |     ; (before optimizer)
                       |     ; finder/SplFileInfo.php:76-76
                       |     ; return  [] RANGE[0..0]
                       |0000 CV0($type) = RECV 1
                       |0001 CV1($msg) = RECV 2
                       |0002 BIND_STATIC (ref) CV2($error)
                       |0003 ASSIGN CV2($error) CV1($msg)
                       |0004 RETURN null
                       |
                       |Symfony\Component\Finder\{closure}:
                       |     ; (lines=5, args=2, vars=3, tmps=1)
                       |     ; (before block pass)
                       |     ; finder/SplFileInfo.php:76-76
                       |     ; return  [] RANGE[0..0]
                       |BB0:
                       |     ; start exit lines=[0-4]
                       |0000 CV0($type) = RECV 1
                       |0001 CV1($msg) = RECV 2
                       |0002 BIND_STATIC (ref) CV2($error)
                       |0003 ASSIGN CV2($error) CV1($msg)
                       |0004 RETURN null
                       |""".stripMargin
      val Parsed.Success(_, length) = parse(fullDump, FileParser8.parseByteCodeDump(_))
      length shouldBe fullDump.length
    }

    "support php8 stuff" should {

      "nullsafe operator" in {
        /*
        generated from:
        <?php
        $a?->f();
         */
        val fullDump = s"""
                         |$$_main:
                         |     ; (lines=9, args=0, vars=1, tmps=1)
                         |     ; (before optimizer)
                         |     ; /home/malte/coding/uni/master/testproject/tests/null.php:1-7
                         |     ; return  [] RANGE[0..0]
                         |0000 EXT_STMT
                         |0001 V1 = JMP_NULL CV0($$a) 0004
                         |0002 INIT_METHOD_CALL 0 CV0($$a) string("${encode("f")}")
                         |0003 DO_FCALL
                         |0004 EXT_STMT
                         |0005 JMPZ CV0($$a) 0008
                         |0006 EXT_STMT
                         |0007 ECHO int(1)
                         |0008 RETURN int(1)
                         |
                         |$$_main:
                         |     ; (lines=9, args=0, vars=1, tmps=1)
                         |     ; (before block pass)
                         |     ; /home/malte/coding/uni/master/testproject/tests/null.php:1-7
                         |     ; return  [] RANGE[0..0]
                         |BB0:
                         |     ; start lines=[0-1]
                         |     ; to=(BB2, BB1)
                         |0000 EXT_STMT
                         |0001 V1 = JMP_NULL CV0($$a) BB2
                         |
                         |BB1:
                         |     ; follow lines=[2-3]
                         |     ; to=(BB2)
                         |0002 INIT_METHOD_CALL 0 CV0($$a) string("${encode("f")}")
                         |0003 DO_FCALL
                         |
                         |BB2:
                         |     ; follow target lines=[4-5]
                         |     ; to=(BB4, BB3)
                         |0004 EXT_STMT
                         |0005 JMPZ CV0($$a) BB4
                         |
                         |BB3:
                         |     ; follow lines=[6-7]
                         |     ; to=(BB4)
                         |0006 EXT_STMT
                         |0007 ECHO int(1)
                         |
                         |BB4:
                         |     ; follow target exit lines=[8-8]
                         |0008 RETURN int(1)
                         |""".stripMargin
        val Parsed.Success(result, count) = parse(fullDump, FileParser8.parseByteCodeDump(_), verboseFailures = true)
        Console.out.flush()
        assert(count == fullDump.length) // parsed everything
        assert(result.length == 1) // resulted in one method
      }

      /**
       * generated from https://www.php.net/manual/en/control-structures.match.php:
       */
      //    <?php
      //    $food = 'cake';
      //
      //    $return_value = match ($food) {
      //    'apple' => 'This food is an apple',
      //    'bar' => 'This food is a bar',
      //    'cake' => 'This food is a cake',
      //    };
      //
      //    var_dump($return_value);
      //    ?>
      "match expression" in {
        val fullDump =
          s"""
            |$$_main:
            |     ; (lines=17, args=0, vars=2, tmps=5)
            |     ; (before optimizer)
            |     ; /home/malte/coding/uni/master/testproject/tests/match.php:1-11
            |     ; return  [] RANGE[0..0]
            |0000 EXT_STMT
            |0001 ASSIGN CV0($$food) string("${encode("cake")}")
            |0002 EXT_STMT
            |0003 MATCH CV0($$food) "${encode("apple")}": 0005, "${encode("bar")}": 0007, "${encode("cake")}": 0009, default: 0004
            |0004 MATCH_ERROR CV0($$food)
            |0005 T4 = QM_ASSIGN string("${encode("apple")}")
            |0006 JMP 0011
            |0007 T4 = QM_ASSIGN string("${encode("bar")}")
            |0008 JMP 0011
            |0009 T4 = QM_ASSIGN string("${encode("cake")}")
            |0010 JMP 0011
            |0011 ASSIGN CV1($$return_value) T4
            |0012 EXT_STMT
            |0013 INIT_FCALL 1 96 string("${encode("var_dump")}")
            |0014 SEND_VAR CV1($$return_value) 1
            |0015 DO_FCALL
            |0016 RETURN int(1)
            |LIVE RANGES:
            |     4: 0010 - 0011 (tmp/var)
            |
            |$$_main:
            |     ; (lines=17, args=0, vars=2, tmps=5)
            |     ; (before block pass)
            |     ; /home/malte/coding/uni/master/testproject/tests/match.php:1-11
            |     ; return  [] RANGE[0..0]
            |BB0:
            |     ; start lines=[0-3]
            |     ; to=(BB2, BB3, BB4, BB1)
            |0000 EXT_STMT
            |0001 ASSIGN CV0($$food) string("${encode("cake")}")
            |0002 EXT_STMT
            |0003 MATCH CV0($$food) "${encode("apple")}": BB2, "${encode("bar")}": BB3, "${encode("cake")}": BB4, default: BB1
            |
            |BB1:
            |     ; target exit lines=[4-4]
            |0004 MATCH_ERROR CV0($$food)
            |
            |BB2:
            |     ; target lines=[5-6]
            |     ; to=(BB5)
            |0005 T4 = QM_ASSIGN string("${encode("apple")}")
            |0006 JMP BB5
            |
            |BB3:
            |     ; target lines=[7-8]
            |     ; to=(BB5)
            |0007 T4 = QM_ASSIGN string("${encode("bar")}")
            |0008 JMP BB5
            |
            |BB4:
            |     ; target lines=[9-10]
            |     ; to=(BB5)
            |0009 T4 = QM_ASSIGN string("${encode("cake")}")
            |0010 NOP
            |
            |BB5:
            |     ; follow target exit lines=[11-16]
            |0011 ASSIGN CV1($$return_value) T4
            |0012 EXT_STMT
            |0013 INIT_FCALL 1 96 string("${encode("var_dump")}")
            |0014 SEND_VAR CV1($$return_value) 1
            |0015 DO_FCALL
            |0016 RETURN int(1)
            |""".stripMargin
        val Parsed.Success(result, count) = parse(fullDump, FileParser8.parseByteCodeDump(_), verboseFailures = true)
        Console.out.flush()
        assert(count == fullDump.length) // parsed everything
        assert(result.length == 1) // resulted in one method
      }
      "undef arguments" in {
        // from https://wiki.php.net/rfc/named_params
        // gen from
        // <?php
        //htmlspecialchars($string, double_encode: false);
        val fullDump = s"""
                         |$$_main:
                         |     ; (lines=7, args=0, vars=1, tmps=1)
                         |     ; (before optimizer)
                         |     ; /home/malte/coding/uni/master/testproject/tests/np.php:1-3
                         |     ; return  [] RANGE[0..0]
                         |0000 EXT_STMT
                         |0001 INIT_FCALL 1 96 string("${encode("htmlspecialchars")}")
                         |0002 SEND_VAR CV0($$string) 1
                         |0003 SEND_VAL bool(false) string("${encode("doublencode")}")
                         |0004 CHECK_UNDEF_ARGS
                         |0005 DO_FCALL
                         |0006 RETURN int(1)
                         |
                         |$$_main:
                         |     ; (lines=7, args=0, vars=1, tmps=1)
                         |     ; (before block pass)
                         |     ; /home/malte/coding/uni/master/testproject/tests/np.php:1-3
                         |     ; return  [] RANGE[0..0]
                         |BB0:
                         |     ; start exit lines=[0-6]
                         |0000 EXT_STMT
                         |0001 INIT_FCALL 1 96 string("${encode("htmlspecialchars")}")
                         |0002 SEND_VAR CV0($$string) 1
                         |0003 SEND_VAL bool(false) string("${encode("doublencode")}")
                         |0004 CHECK_UNDEF_ARGS
                         |0005 DO_FCALL
                         |0006 RETURN int(1)
                         |""".stripMargin
        val Parsed.Success(result, count) = parse(fullDump, FileParser8.parseByteCodeDump(_), verboseFailures = true)
        Console.out.flush()
        assert(count == fullDump.length) // parsed everything
        assert(result.length == 1) // resulted in one method
      }

      "support many features at once" in {
        val cwd = "pwd".!!.stripMargin.trim
        val methodDefinitionPairs =
          FileParser8.parseFromFile(new File(
            cwd + "/layerByteCode/resources/integrationtesting/php8_1.php"), config.php8.interpreter, config.php8.phpini)
        methodDefinitionPairs.length shouldBe 2
      }
    }
  }

  "ByteCodeParser.parse" should {
    "be able to directly deal with a php file" in {
      val cwd = "pwd".!!.stripMargin.trim
      val methodDefinitionPairs =
        FileParser8.parseFromFile(new File(
          cwd + "/layerByteCode/resources/unittesting/testprojects/onlyMainCreation/trivial-php.php"), config.php8.interpreter, config.php8.phpini)
      assert(methodDefinitionPairs.length == 1)
      assert(methodDefinitionPairs.head.byteCodeBlock.instructions.length == 6)
    }
    "be able to extract multiple methods from the same file if they exist" in {
      val cwd = "pwd".!!.stripMargin.trim
      val multipleMethodsPairs =
        FileParser8.parseFromFile(new File(
          cwd + "/layerByteCode/resources/unittesting/testprojects/twoFunctionsAndMain/main.php"), config.php8.interpreter, config.php8.phpini)
      assert(multipleMethodsPairs.length == 3)
    }
    "be able to extract multiple BB in basicConditional project" in {
      val cwd = "pwd".!!.stripMargin.trim
      val multipleMethodPairs = FileParser8.parseFromFile(new File(
        cwd + "/layerByteCode/resources/unittesting/testprojects/basicConditional/main.php"), config.php8.interpreter, config.php8.phpini)
      multipleMethodPairs.length shouldBe 1
      multipleMethodPairs.head.controlFlowBlock.blocks.length shouldBe 3
      // php8 pulls the return int (1) into each conditonal BB, resulting in one less BB
    }
    "be able to extract multiple defintion pairs in project" in {
      val cwd = "pwd".!!.stripMargin.trim
      val multipleMethodPairs = FileParser8.parseFromFile(new File(
        cwd + "/layerByteCode/resources/unittesting/testprojects/singleClassProject/main.php"), config.php8.interpreter, config.php8.phpini)
      multipleMethodPairs.length shouldBe 4
    }
  }
  "be able to deal with unicode" when {
    "in identifier" in {
      val cwd = "pwd".!!.stripMargin.trim
      try {
        FileParser8.parseFromFile(new File(
          cwd + "/layerByteCode/resources/unittesting/testprojects/unicode/main.php"), config.php8.interpreter, config.php8.phpini)
      } catch {
        case x: Throwable =>
          fail(x.getMessage)
      }
    }
    "in function name" in {
      val cwd = "pwd".!!.stripMargin.trim
      try {
        val res = FileParser8.parseFromFile(new File(
          cwd + "/layerByteCode/resources/unittesting/testprojects/unicode/hiragana.php"), config.php8.interpreter, config.php8.phpini)
        res.map(_.byteCodeBlock.name).toSet shouldBe Set("dlr_main", "rさ")
      } catch {
        case x: Throwable =>
          fail(x.getMessage)
      }
    }
  }
}
