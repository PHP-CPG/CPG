package io.joern.bytecode.parser.php8

import fastparse._
import io.joern.bytecode.parser.EasyBase64.encode
import io.joern.bytecode.parser.constructs._
import io.joern.bytecode.parser.php8.Instructions._
import io.joern.bytecode.parser.php8.instructions.ControlConstructs.{parseNumberDestinationPattern, parseStringDestinationPattern}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class InstructionsTest extends AnyWordSpec with Matchers {

  "parser getOperation" should {
    "be able to parse NEW 1 string(\"Basic\")" in {
      val operation = s"""NEW 1 string("${encode("Basic")}")"""
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case DualValueOperation(command, lhs, rhs) =>
          command shouldBe "NEW"
          lhs match {
            case IntegerLiteral(value) => value shouldBe 1
            case _ => fail(message = "lhs is not of type IntegerLiteral")
          }
          rhs match {
            case StringLiteral(value) => value shouldBe "Basic"
            case _ => fail(message = "rhs is not of type StringLiteral")
          }

        case _ => fail(message = "result is not of type DualValueOperation")
      }
    }
    "be able to parse INIT_NS_FCALL_BY_NAME 2 string(\"some\\qualified\\name\")" in {
      val operation =
        s"""INIT_NS_FCALL_BY_NAME 2 string("${encode("some\\qualified\\name")}")"""
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case INIT_NS_FCALL_BY_NAME(paramCount, function) =>
          paramCount shouldBe 2
          function shouldBe "some\\qualified\\name"
        case _ => fail(message = "return is not of type INIT_NS_FCALL_BY_NAME")
      }
    }
    "be able to parse INIT_NS_FCALL_BY_NAME 1 string(\"PleskX\\Api\\get_class\")" in {
      val operation =
        s"""INIT_NS_FCALL_BY_NAME 1 string("${encode("PleskX\\Api\\get_class")}")"""
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case INIT_NS_FCALL_BY_NAME(paramCount, function) =>
          paramCount shouldBe 1
          function shouldBe "pleskx\\api\\get_class"
        case _ => fail(message = "return is not of type INIT_NS_FCALL_BY_NAME")
      }
    }
    "be able to parse INIT_METHOD_CALL 1 CV0($var) string(\"test\")" in {
      val operation = s"""INIT_METHOD_CALL 1 CV0($$var) string("${encode("test")}")"""
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case INIT_METHOD_CALL(paramCount, objectVar, method) =>
          paramCount shouldBe 1
          val Variable(name, tmp, ref) = objectVar
          name shouldBe "var"
          tmp shouldBe false
          ref shouldBe false
          method shouldBe StringLiteral("test")
        case _ => fail(message = "result is not of type INIT_METHOD_CALL")
      }
    }
    "be able to parse INIT_METHOD_CALL 1 THIS string(\"test\")" in {
      val operation = s"""INIT_METHOD_CALL 1 THIS string("${encode("test")}")"""
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case INIT_METHOD_CALL(paramCount, objectVar, method) =>
          paramCount shouldBe 1
          val Variable(name, tmp, ref) = objectVar
          name shouldBe "THIS"
          tmp shouldBe false
          ref shouldBe true
          method shouldBe StringLiteral("test")
        case _ => fail(message = "result is not of type INIT_METHOD_CALL")
      }
    }
    "be able to parse INIT_STATIC_METHOD_CALL 3 string(\"test\") CONSTRUCTOR" in {
      val operation = s"""INIT_STATIC_METHOD_CALL 3 string("${encode("test")}") CONSTRUCTOR"""
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op shouldBe INIT_STATIC_METHOD_CALL(3, None, None, Some(StringLiteral("test")), ByteCodeConstructor())
    }
    "be able to parse INIT_FCALL 2 42 string(\"phpinfo\")" in {
      val operation = s"""INIT_FCALL 2 42 string("${encode("phpinfo")}")"""
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case INIT_FCALL(parameter, unknown, name) =>
          parameter shouldBe 2
          unknown shouldBe 42
          name match {
            case StringLiteral(value) => value shouldBe "phpinfo"
            case _ => fail(message = "the name is not of type StringLiteral")
          }

        case _ => fail(message = "the result is not of type INIT_FCALL")
      }
    }
    "be able to parse INIT_DYNAMIC_CALL 2 CV($x)" in {
      val operation = "INIT_DYNAMIC_CALL 2 CV($x)"
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case INIT_DYNAMIC_CALL(paramCount, variable) =>
          paramCount shouldBe 2
          variable match {
            case Variable(name, tmp, reference) =>
              name shouldBe "x"
              tmp shouldBe false
              reference shouldBe false
            case _ => fail(message = "the name is not of type StringLiteral")
          }

        case _ => fail(message = "the result is not of type INIT_FCALL")
      }
    }
    "be able to parse INIT_ARRAY 0 NEXT" in {
      val operation = "INIT_ARRAY 0 NEXT"
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case DualValueOperation(code, pos, first) =>
          code shouldBe "INIT_ARRAY"
          pos shouldBe IntegerLiteral(0)
          first shouldBe ByteCodePlaceIndicator("NEXT")
        case x => fail(s"unexpected operation $x")
      }
    }
    "be able to parse INIT_ARRAY 1 (packed) CV0($request) CV2($wrapped)" in {
      val operation = "INIT_ARRAY 1 (packed) CV0($request) CV2($wrapped)"
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case QuadrupleValueOperation(code, first, second, third, fourth) =>
          code shouldBe "INIT_ARRAY"
          first shouldBe IntegerLiteral(1)
          second shouldBe ByteCodeKeyword("packed")
          third shouldBe Variable("request", tmp = false)
          fourth shouldBe Variable("wrapped", tmp = false)
        case x => fail(s"unexpected operation $x")
      }
    }
    "be able to parse INIT_FCALL_BY_NAME 2 string(\"phpinfo\")" in {
      val operation = s"""INIT_FCALL_BY_NAME 2 string("${encode("phpinfo")}")"""
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case INIT_FCALL_BY_NAME(paramCount, name) =>
          paramCount shouldBe 2
          name shouldBe "phpinfo"
        case _ => fail(message = "the result is not of type INIT_FCALL")
      }
    }
    "be able to parse SEND_VAR_EX CV($var) 2" in {
      val operation = "SEND_VAR_EX CV($var) 2"
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case DualValueOperation(command, lhs, rhs) =>
          command shouldBe "SEND_VAR_EX"
          lhs match {
            case Variable(name, tmp, reference) =>
              name shouldBe "var"
              tmp shouldBe false
              reference shouldBe false
            case _ => fail(message = "lhs is not of type Variable")
          }
          rhs match {
            case IntegerLiteral(value) => value shouldBe 2
            case _ => fail(message = "rhs is not of type IntegerLiteral")
          }
        case _ => fail(message = "result is not of type DualValueOperation")
      }
    }
    "be able to parse SEND_VAL_EX CV($var) 2" in {
      val operation = "SEND_VAL_EX CV($var) 2"
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case DualValueOperation(command, lhs, rhs) =>
          command shouldBe "SEND_VAL_EX"
          lhs match {
            case Variable(name, tmp, reference) =>
              name shouldBe "var"
              tmp shouldBe false
              reference shouldBe false
            case _ => fail(message = "lhs is not of type Variable")
          }
          rhs match {
            case IntegerLiteral(value) => value shouldBe 2
            case _ => fail(message = "rhs is not of type IntegerLiteral")
          }
        case _ => fail(message = "result is not of type DualValueOperation")
      }
    }
    "be able to parse CHECK_UNDEF_ARGS" in {
      val operation = "CHECK_UNDEF_ARGS"
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op shouldBe NoValueOperation("CHECK_UNDEF_ARGS")
    }

    "be able to parse FETCH_CLASS_CONSTANT string(\"PleskX\\Api\\Client\") string(\"RESPONSE_FULL\")" in {
      val operation =
        s"""FETCH_CLASS_CONSTANT string("${encode("PleskX\\Api\\Client")}") string("${encode("RESPONSE_FULL")}")"""
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case DualValueOperation(code, lhs, rhs) =>
          code shouldBe "FETCH_CLASS_CONSTANT"
          lhs match {
            case StringLiteral(value) => value shouldBe "PleskX\\Api\\Client"
            case x => fail(s"unexpected value $x")
          }
          rhs match {
            case StringLiteral(value) => value shouldBe "RESPONSE_FULL"
            case x => fail(s"unexpected value $x")
          }
        case x => fail(s"unexpected operation $x")
      }
    }
    "be able to parse FETCH_CONSTANT (unqualified-in-namespace) string(\"Workerman\\Protocols\\PHP_URL_QUERY\")" in {
      val op = "FETCH_CONSTANT (unqualified-in-namespace) string(\"" + encode("""Workerman\Protocols\PHP_URL_QUERY""") + "\")"
      val Parsed.Success(result, length) = parse(op, getOperation(_))
      length shouldBe op.length
      result.op shouldBe DualValueOperation("FETCH_CONSTANT",
        ByteCodeKeyword("unqualified-in-namespace"),
        StringLiteral("Workerman\\Protocols\\PHP_URL_QUERY")
      )
    }
    "be able to parse FETCH_STATIC_PROP_W string(\"app\") string(\"Yii\")" in {
      val op = "FETCH_STATIC_PROP_W string(\"" + encode("app") + "\") string(\"" + encode("Yii") + "\")"
      val Parsed.Success(result, length) = parse(op, getOperation(_))
      length shouldBe op.length
      result.op shouldBe DualValueOperation("FETCH_STATIC_PROP_W", StringLiteral("app"), StringLiteral("Yii"))
    }

    "be able to parse RECV 2" in {
      val operation = "RECV 2"
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case SingleValueOperation(code, value) =>
          code shouldBe "RECV"
          value match {
            case IntegerLiteral(value) => value shouldBe 2
            case _ => fail(message = "the value is not of type IntegerLiteral")
          }
        case _ =>
          fail(message = "the result is not of type SingleValueOperation")
      }
    }
    "be able to parse RECV_INIT 4 string(\"test\")" in {
      val operation = s"""RECV_INIT 4 string("${encode("test")}")"""
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case DualValueOperation(code, lhs, rhs) =>
          code shouldBe "RECV_INIT"
          lhs match {
            case IntegerLiteral(value) => value shouldBe 4
            case _ => fail(message = "lhs is not of type IntegerLiteral")
          }
          rhs match {
            case StringLiteral(value) => value shouldBe "test"
            case _ => fail(message = "rhs is not of type StringLiteral")
          }
        case _ => fail(message = "result is not of type DualOperation")
      }
    }
    "be able to parse SEND_VAL string(\"string\") 1" in {
      val operation = s"""SEND_VAL string("${encode("string")}") 1"""
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case DualValueOperation(code, lhs, rhs) =>
          code shouldBe "SEND_VAL"
          lhs match {
            case StringLiteral(value) => value shouldBe "string"
            case _ => fail(message = "lhs is not of type StringLiteral")
          }
          rhs match {
            case IntegerLiteral(value) => value shouldBe 1
            case _ => fail(message = "rhs is not of type IntegerLiteral")
          }
        case _ => fail(message = "result is not of type DualValueOperation")
      }
    }
    "be able to parse JMP 0001" in {
      val operation = "JMP 0001"
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case SingleValueOperation(code, value) =>
          code shouldBe "JMP"
          value match {
            case IntegerLiteral(value) => value shouldBe 1
            case _ => fail(message = "rhs is not of type IntegerLiteral")
          }
        case _ => fail(message = "result is not of type DualValueOperation")
      }
    }
    "be able to parse JMP BB1" in {
      val operation = "JMP BB1"
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case SingleValueOperation(code, value) =>
          code shouldBe "JMP"
          value match {
            case IntegerLiteral(value) => value shouldBe 1
            case _ => fail(message = "rhs is not of type IntegerLiteral")
          }
        case _ => fail(message = "result is not of type DualValueOperation")
      }
    }
    "be able to parse JMPZ CV($var) 0001" in {
      val operation = "JMPZ CV($var) 0001"
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case DualValueOperation(code, lhs, rhs) =>
          code shouldBe "JMPZ"
          lhs match {
            case Variable(name, tmp, reference) =>
              name shouldBe "var"
              tmp shouldBe false
              reference shouldBe false
            case _ => fail(message = "lhs is not of type Variable")
          }
          rhs match {
            case IntegerLiteral(value) => value shouldBe 1
            case _ => fail(message = "rhs is not of type IntegerLiteral")
          }
        case _ => fail(message = "result is not of type DualValueOperation")
      }
    }
    "be able to parse JMPZ CV($var) BB1" in {
      val operation = "JMPZ CV($var) BB1"
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case DualValueOperation(code, lhs, rhs) =>
          code shouldBe "JMPZ"
          lhs match {
            case Variable(name, tmp, reference) =>
              name shouldBe "var"
              tmp shouldBe false
              reference shouldBe false
            case _ => fail(message = "lhs is not of type Variable")
          }
          rhs match {
            case IntegerLiteral(value) => value shouldBe 1
            case _ => fail(message = "rhs is not of type IntegerLiteral")
          }
        case _ => fail(message = "result is not of type DualValueOperation")
      }
    }
    "be able to parse JMPNZ CV($var) 0001" in {
      val operation = "JMPNZ CV($var) 0001"
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case DualValueOperation(code, lhs, rhs) =>
          code shouldBe "JMPNZ"
          lhs match {
            case Variable(name, tmp, reference) =>
              name shouldBe "var"
              tmp shouldBe false
              reference shouldBe false
            case _ => fail(message = "lhs is not of type Variable")
          }
          rhs match {
            case IntegerLiteral(value) => value shouldBe 1
            case _ => fail(message = "rhs is not of type IntegerLiteral")
          }
        case _ => fail(message = "result is not of type DualValueOperation")
      }
    }
    "be able to parse JMPNZ CV($var) BB1" in {
      val operation = "JMPNZ CV($var) BB1"
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case DualValueOperation(code, lhs, rhs) =>
          code shouldBe "JMPNZ"
          lhs match {
            case Variable(name, tmp, reference) =>
              name shouldBe "var"
              tmp shouldBe false
              reference shouldBe false
            case _ => fail(message = "lhs is not of type Variable")
          }
          rhs match {
            case IntegerLiteral(value) => value shouldBe 1
            case _ => fail(message = "rhs is not of type IntegerLiteral")
          }
        case _ => fail(message = "result is not of type DualValueOperation")
      }
    }
    "be able to parse JMPNZ_EX CV($var) 0001" in {
      val operation = "JMPNZ_EX CV($var) 0001"
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case DualValueOperation(code, lhs, rhs) =>
          code shouldBe "JMPNZ_EX"
          lhs match {
            case Variable(name, tmp, reference) =>
              name shouldBe "var"
              tmp shouldBe false
              reference shouldBe false
            case _ => fail(message = "lhs is not of type Variable")
          }
          rhs match {
            case IntegerLiteral(value) => value shouldBe 1
            case _ => fail(message = "rhs is not of type IntegerLiteral")
          }
        case _ => fail(message = "result is not of type DualValueOperation")
      }
    }
    "be able to parse JMPNZ_EX CV($var) BB1" in {
      val operation = "JMPNZ_EX CV($var) BB1"
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case DualValueOperation(code, lhs, rhs) =>
          code shouldBe "JMPNZ_EX"
          lhs match {
            case Variable(name, tmp, reference) =>
              name shouldBe "var"
              tmp shouldBe false
              reference shouldBe false
            case _ => fail(message = "lhs is not of type Variable")
          }
          rhs match {
            case IntegerLiteral(value) => value shouldBe 1
            case _ => fail(message = "rhs is not of type IntegerLiteral")
          }
        case _ => fail(message = "result is not of type DualValueOperation")
      }
    }
    "be able to parse JMP_NULL CV0($a) 0004" in {
      val operation = "JMP_NULL CV0($a) 0004"
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case DualValueOperation(code, lhs, rhs) =>
          code shouldBe "JMP_NULL"
          lhs match {
            case Variable(name, tmp, reference) =>
              name shouldBe "a"
              tmp shouldBe false
              reference shouldBe false
            case _ => fail(message = "lhs is not of type Variable")
          }
          rhs match {
            case IntegerLiteral(value) => value shouldBe 4
            case _ => fail(message = "rhs is not of type IntegerLiteral")
          }
        case _ => fail(message = "result is not of type DualValueOperation")
      }
    }
    "be able to parse FE_RESET_R V1 0001" in {
      val operation = "FE_RESET_R V1 0001"
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case DualValueOperation(code, lhs, rhs) =>
          code shouldBe "FE_RESET_R"
          lhs match {
            case Variable(name, tmp, reference) =>
              name shouldBe "V1"
              tmp shouldBe true
              reference shouldBe true
            case _ => fail(message = "lhs is not of type Variable")
          }
          rhs match {
            case IntegerLiteral(value) => value shouldBe 1
            case _ => fail(message = "rhs is not of type IntegerLiteral")
          }
        case _ => fail(message = "result is not of type DualOperation")
      }
    }
    "be able to parse FE_RESET_R V1 BB1" in {
      val operation = "FE_RESET_R V1 BB1"
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case DualValueOperation(code, lhs, rhs) =>
          code shouldBe "FE_RESET_R"
          lhs match {
            case Variable(name, tmp, reference) =>
              name shouldBe "V1"
              tmp shouldBe true
              reference shouldBe true
            case _ => fail(message = "lhs is not of type Variable")
          }
          rhs match {
            case IntegerLiteral(value) => value shouldBe 1
            case _ => fail(message = "rhs is not of type IntegerLiteral")
          }
        case _ => fail(message = "result is not of type DualOperation")
      }
    }
    "be able to parse FE_FETCH_R V1 CV($array) 0001" in {
      val operation = "FE_FETCH_R V1 CV($var) 0001"
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case TripleValueOperation(code, first, second, third) =>
          code shouldBe "FE_FETCH_R"
          first match {
            case Variable(name, tmp, reference) =>
              name shouldBe "V1"
              tmp shouldBe true
              reference shouldBe true
            case _ => fail(message = "first value is not of type Variable")
          }
          second match {
            case Variable(name, tmp, reference) =>
              name shouldBe "var"
              tmp shouldBe false
              reference shouldBe false
            case _ => fail(message = "second value is not of type Variable")
          }
          third match {
            case IntegerLiteral(value) => value shouldBe 1
            case _ =>
              fail(message = "third value is not of type IntegerLiteral")
          }
        case _ => fail(message = "result is not of type TripleValueOperation")
      }
    }
    "be able to parse FE_FETCH_R V1 CV($array) BB1" in {
      val operation = "FE_FETCH_R V1 CV($var) BB1"
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case TripleValueOperation(code, first, second, third) =>
          code shouldBe "FE_FETCH_R"
          first match {
            case Variable(name, tmp, reference) =>
              name shouldBe "V1"
              tmp shouldBe true
              reference shouldBe true
            case _ => fail(message = "first value is not of type Variable")
          }
          second match {
            case Variable(name, tmp, reference) =>
              name shouldBe "var"
              tmp shouldBe false
              reference shouldBe false
            case _ => fail(message = "second value is not of type Variable")
          }
          third match {
            case IntegerLiteral(value) => value shouldBe 1
            case _ =>
              fail(message = "third value is not of type IntegerLiteral")
          }
        case _ => fail(message = "result is not of type TripleValueOperation")
      }
    }
    "be able to parse FETCH_DIM_R V1 string(\"a\")" in {
      val operation = s"""FETCH_DIM_R V1 string("${encode("a")}")"""
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case DualValueOperation(code, lhs, rhs) =>
          code shouldBe "FETCH_DIM_R"
          lhs match {
            case Variable(name, tmp, reference) =>
              name shouldBe "V1"
              tmp shouldBe true
              reference shouldBe true
            case _ => fail(message = "lhs is not of type Variable")
          }
          rhs match {
            case StringLiteral(value) => value shouldBe "a"
            case _ => fail(message = "rhs is not of type StringLiteral")
          }
        case _ => fail(message = "result is not of type DualValueOperation")
      }
    }
    "be able to parse ASSIGN_DIM CV($array) NEXT" in {
      val operation = "ASSIGN_DIM CV($array) NEXT"
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case DualValueOperation(code, lhs, rhs) =>
          code shouldBe "ASSIGN_DIM"
          lhs match {
            case Variable(name, tmp, reference) =>
              name shouldBe "array"
              tmp shouldBe false
              reference shouldBe false
            case _ => fail(message = "lhs is not of type Variable")
          }
          rhs match {
            case ByteCodePlaceIndicator(value) => value shouldBe "NEXT"
            case _ => fail(message = "rhs is not of type StringLiteral")
          }
        case _ => fail(message = "result is not of type DualValueOperation")
      }
    }
    "be able to parse ASSIGN_DIM CV($array) int(1)" in {
      val operation = "ASSIGN_DIM CV($array) int(1)"
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case DualValueOperation(code, lhs, rhs) =>
          code shouldBe "ASSIGN_DIM"
          lhs match {
            case Variable(name, tmp, reference) =>
              name shouldBe "array"
              tmp shouldBe false
              reference shouldBe false
            case _ => fail(message = "lhs is not of type Variable")
          }
          rhs match {
            case IntegerLiteral(value) => value shouldBe 1
            case _ => fail(message = "rhs is not of type IntegerLiteral")
          }
        case _ => fail(message = "result is not of type DualValueOperation")
      }
    }
    "be able to parse ASSIGN CV($x) int(42)" in {
      val operation = "ASSIGN CV($x) int(42)"
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case DualValueOperation(code, lhs, rhs) =>
          code shouldBe "ASSIGN"
          lhs match {
            case Variable(name, tmp, reference) =>
              name shouldBe "x"
              tmp shouldBe false
              reference shouldBe false
            case _ => fail(message = "lhs is not of type Variable")
          }
          rhs match {
            case IntegerLiteral(value) => value shouldBe 42
            case _ => fail(message = "rhs is not of type IntegerLiteral")
          }
        case _ => fail(message = "result is not of type DualValueOperation")
      }
    }
    "be able to parse ASSIGN_OBJ_OP (CONCAT) V11 int(42)" in {
      val operation = "ASSIGN_OBJ_OP (CONCAT) V11 int(42)"
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case TripleValueOperation(code, first, second, _) =>
          code shouldBe "ASSIGN_OBJ_OP"
          first shouldBe AssignOpLiteral("CONCAT")
          second shouldBe Variable("V11", tmp = true, reference = true)
        case x => fail(s"unexpected operation $x")
      }
    }
    "be able to parse ROPE_INIT 3 string(\"a\")" in {
      val operation = s"""ROPE_INIT 3 string("${encode("a")}")"""
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case DualValueOperation(code, lhs, rhs) =>
          code shouldBe "ROPE_INIT"
          lhs match {
            case IntegerLiteral(value) => value shouldBe 3
            case _ => fail(message = "lhs is not of type IntegerLiteral")
          }
          rhs match {
            case StringLiteral(value) => value shouldBe "a"
            case _ => fail(message = "rhs is not of type StringLiteral")
          }
        case _ => fail(message = "the result is not of type DualValueOperation")
      }
    }
    "be able to parse ROPE_ADD 3 CV($a) T1" in {
      val operation = "ROPE_ADD 3 CV($a) T1"
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case TripleValueOperation(code, first, second, third) =>
          code shouldBe "ROPE_ADD"
          first match {
            case IntegerLiteral(value) => value shouldBe 3
            case _ => fail(message = "first is not of type IntegerLiteral")
          }
          second match {
            case Variable(name, tmp, reference) =>
              name shouldBe "a"
              tmp shouldBe false
              reference shouldBe false
            case _ => fail(message = "second is not of type Variable")
          }
          third match {
            case Variable(name, tmp, reference) =>
              name shouldBe "T1"
              tmp shouldBe true
              reference shouldBe false
            case _ => fail(message = "third is not of type Variable")
          }
        case _ => fail(message = "result is not of type TripleValueOperation")
      }
    }
    "be able to parse ROPE_END 2 T1 T2" in {
      val operation = "ROPE_END 2 T1 T2"
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case TripleValueOperation(code, first, second, third) =>
          code shouldBe "ROPE_END"
          first match {
            case IntegerLiteral(value) => value shouldBe 2
            case _ => fail(message = "first is not of type IntegerLiteral")
          }
          second match {
            case Variable(name, tmp, reference) =>
              name shouldBe "T1"
              tmp shouldBe true
              reference shouldBe false
            case _ => fail(message = "second is not of type Variable")
          }
          third match {
            case Variable(name, tmp, reference) =>
              name shouldBe "T2"
              tmp shouldBe true
              reference shouldBe false
            case _ => fail(message = "second is not of type Variable")
          }
        case _ => fail(message = "result is not of type TripleValueOperation")
      }
    }
    "be able to parse ASSIGN_OP (ADD) CV($a) CV($b)" in {
      val operation = "ASSIGN_OP (ADD) CV($a) CV($b)"
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case TripleValueOperation(code, first, second, third) =>
          code shouldBe "ASSIGN_OP"
          first match {
            case AssignOpLiteral(value) => value shouldBe "ADD"
            case _ => fail(message = "first is not of type StringLiteral")
          }
          second match {
            case Variable(name, tmp, reference) =>
              name shouldBe "a"
              tmp shouldBe false
              reference shouldBe false
            case _ => fail(message = "second is not of type Variable")
          }
          third match {
            case Variable(name, tmp, reference) =>
              name shouldBe "b"
              tmp shouldBe false
              reference shouldBe false
            case _ => fail(message = "second is not of type Variable")
          }
        case _ => fail(message = "result is not of type TripleValueOperation")
      }
    }
    "be able to parse all ASSIGN_OP OPs" in {
      def variant(x: String): Unit = {
        val Parsed.Success(_, length) = parse(x, getOperation(_))
        length shouldBe x.length
      }

      variant("ASSIGN_OP (SUB) CV0($a) int(2)")
      variant("ASSIGN_OP (MUL) CV0($a) int(2)")
      variant("ASSIGN_OP (ADD) CV0($a) int(2)")
      variant("ASSIGN_OP (DIV) CV0($a) int(2)")
      variant("ASSIGN_OP (MOD) CV0($a) int(2)")
      variant("ASSIGN_OP (POW) CV0($a) int(2)")
      variant("ASSIGN_OP (ADD) CV0($a) int(2)")
      variant("ASSIGN_OP (BW_AND) CV0($a) int(2)")
      variant("ASSIGN_OP (BW_OR) CV0($a) int(2)")
      variant("ASSIGN_OP (BW_XOR) CV0($a) int(2)")
      variant("ASSIGN_OP (SL) CV0($a) int(2)")
      variant("ASSIGN_OP (SR) CV0($a) int(2)")
    }

    "be able to parse all ASSIGN_DIM_OP OPs" in {
      def variant(x: String): Unit = {
        val Parsed.Success(_, length) = parse(x, getOperation(_))
        length shouldBe x.length
      }

      variant("ASSIGN_DIM_OP (SUB) CV0($a) int(0)")
      variant("ASSIGN_DIM_OP (MUL) CV0($a) int(0)")
      variant("ASSIGN_DIM_OP (ADD) CV0($a) int(0)")
      variant("ASSIGN_DIM_OP (DIV) CV0($a) int(0)")
      variant("ASSIGN_DIM_OP (MOD) CV0($a) int(0)")
      variant("ASSIGN_DIM_OP (POW) CV0($a) int(0)")
      variant("ASSIGN_DIM_OP (ADD) CV0($a) int(0)")
      variant("ASSIGN_DIM_OP (BW_AND) CV0($a) int(0)")
      variant("ASSIGN_DIM_OP (BW_OR) CV0($a) int(0)")
      variant("ASSIGN_DIM_OP (BW_XOR) CV0($a) int(0)")
      variant("ASSIGN_DIM_OP (SL) CV0($a) int(0)")
      variant("ASSIGN_DIM_OP (SR) CV0($a) int(0)")
    }
    // below is the representative test for no value operations
    "be able to parse DO_ICALL" in {
      val operation = "DO_ICALL"
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case NoValueOperation(code) => code shouldBe "DO_ICALL"
        case _ => fail(message = "the result is not of type NoValueOperation")
      }
    }
    // below is the representative test for single value operations
    "be able to parse ECHO int(32)" in {
      val operation = "ECHO int(32)"
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case SingleValueOperation(code, value) =>
          code shouldBe "ECHO"
          value match {
            case IntegerLiteral(value) => value shouldBe 32
            case _ => fail(message = "value is not of type IntegerLiteral")
          }
        case x => fail(s"unexpected operation $x")
      }
    }
    // below is the representative test for dual value operations
    "be able to parse CONCAT string(\"a\") string(\"b\")" in {
      val operation = s"""CONCAT string("${encode("a")}") string("${encode("b")}")"""
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      result.op match {
        case DualValueOperation(command, lhs, rhs) =>
          command shouldBe "CONCAT"
          length shouldBe operation.length
          lhs match {
            case StringLiteral(value) => value shouldBe "a"
            case _ => fail(message = "lhs is not of type StringLiteral")
          }
          rhs match {
            case StringLiteral(value) => value shouldBe "b"
            case _ => fail(message = "rhs is not of type StringLiteral")
          }
        case _ => fail(message = "result is not of type DualValueOperation")
      }
    }
    "be able to parse CONCAT string(\"a\") T1" in {
      val operation = s"""CONCAT string("${encode("a")}") T1"""
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      result.op match {
        case DualValueOperation(command, lhs, rhs) =>
          command shouldBe "CONCAT"
          length shouldBe operation.length
          lhs match {
            case StringLiteral(value) => value shouldBe "a"
            case _ => fail(message = "lhs was not of type StringLiteral")
          }
          rhs match {
            case Variable(name, tmp, reference) =>
              name shouldBe "T1"
              tmp shouldBe true
              reference shouldBe false
            case _ => fail(message = "rhs was not of type Variable")
          }
        case _ =>
          fail(message = "the result was not of type DualValueOperation")
      }
    }
    "be able to parse CONCAT string(\"a\") CV($var)" in {
      val operation = s"""CONCAT string("${encode("a")}") CV($$var)"""
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      result.op match {
        case DualValueOperation(command, lhs, rhs) =>
          command shouldBe "CONCAT"
          length shouldBe operation.length
          lhs match {
            case StringLiteral(value) => value shouldBe "a"
            case _ => fail(message = "lhs was not of type StringLiteral")
          }
          rhs match {
            case Variable(name, tmp, reference) =>
              name shouldBe "var"
              tmp shouldBe false
              reference shouldBe false
            case _ => fail(message = "lhs was not of type Variable")
          }
        case _ => fail(message = "lhs was not of type DualValueOperation")
      }
    }
    "be able to parse CONCAT T1 T2" in {
      val operation = "CONCAT T1 T2"
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      result.op match {
        case DualValueOperation(command, lhs, rhs) =>
          command shouldBe "CONCAT"
          length shouldBe operation.length
          lhs match {
            case Variable(name, tmp, reference) =>
              name shouldBe "T1"
              tmp shouldBe true
              reference shouldBe false
            case _ => fail(message = "lhs was not of type variable")
          }
          rhs match {
            case Variable(name, tmp, reference) =>
              name shouldBe "T2"
              tmp shouldBe true
              reference shouldBe false
            case _ => fail(message = "lhs was not of type variable")
          }
        case _ => fail(message = "result was not of type DualValueOperation")
      }
    }
    "be able to parse CONCAT CV($var1) CV($var2)" in {
      val operation = "CONCAT CV($var1) CV($var2)"
      val Parsed.Success(result, length) =
        parse(input = operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case DualValueOperation(command, lhs, rhs) =>
          command shouldBe "CONCAT"
          lhs match {
            case Variable(name, tmp, reference) =>
              name shouldBe "var1"
              tmp shouldBe false
              reference shouldBe false
            case _ => fail(message = "lhs was not of type variable")
          }
          rhs match {
            case Variable(name, tmp, reference) =>
              name shouldBe "var2"
              tmp shouldBe false
              reference shouldBe false
            case _ => fail(message = "lhs was not of type variable")
          }
        case _ => fail(message = "result was not of type DualValueOperation")
      }
    }
    "be able to parse FETCH_STATIC_PROP_FUNC_ARG string(\"test\") (static) (exception)" in {
      val operation =
        s"""FETCH_STATIC_PROP_FUNC_ARG string("${encode("test")}") (static) (exception)"""
      val Parsed.Success(result, length) =
        parse(input = operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case TripleValueOperation(code, first, second, third) =>
          code shouldBe "FETCH_STATIC_PROP_FUNC_ARG"
          first match {
            case StringLiteral(value) => value shouldBe "test"
            case x => fail(s"unexpected value $x")
          }
          second match {
            case ByteCodeKeyword(value) => value shouldBe "static"
            case x => fail(s"unexpected value $x")
          }
          third match {
            case ByteCodeKeyword(value) => value shouldBe "exception"
            case x => fail(s"unexpected value $x")
          }
        case x => fail(s"unexpected op $x")
      }
    }
    "be able to parse INIT_ARRAY 3 (packed) T19 NEXT" in {
      val operation = "INIT_ARRAY 3 (packed) T19 NEXT"
      val Parsed.Success(result, length) =
        parse(input = operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case QuadrupleValueOperation(code, first, second, third, fourth) =>
          code shouldBe "INIT_ARRAY"
          first match {
            case IntegerLiteral(value) => value shouldBe 3
            case x => fail(s"unexpected value $x")
          }
          second match {
            case ByteCodeKeyword(value) => value shouldBe "packed"
            case x => fail(s"unexpected value $x")
          }
          third match {
            case Variable(name, tmp, reference) =>
              name shouldBe "T19"
              tmp shouldBe true
              reference shouldBe false
            case x => fail(s"unexpected value $x")
          }
          fourth match {
            case ByteCodePlaceIndicator(value) => value shouldBe "NEXT"
            case x => fail(s"unexpected value $x")
          }
        case x => fail(s"unexpected op $x")
      }
    }
    "be able to parse FETCH_STATIC_PROP_R string(\"test\") (self) (exception)" in {
      val operation = s"""FETCH_STATIC_PROP_R string("${encode("test")}") (self) (exception)"""
      val Parsed.Success(result, length) =
        parse(input = operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case TripleValueOperation(code, first, second, third) =>
          code shouldBe "FETCH_STATIC_PROP_R"
          first match {
            case StringLiteral(value) => value shouldBe "test"
            case x => fail(s"unexpected value $x")
          }
          second match {
            case ByteCodeKeyword(value) => value shouldBe "self"
            case x => fail(s"unexpected value $x")
          }
          third match {
            case ByteCodeKeyword(value) => value shouldBe "exception"
            case x => fail(s"unexpected value $x")
          }
        case x => fail(s"unexpected op $x")
      }
    }
    "be able to parse FETCH_OBJ_R THIS string(\"test\")" in {
      val operation = s"""FETCH_OBJ_R THIS string("${encode("test")}")"""
      val Parsed.Success(result, length) =
        parse(input = operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case DualValueOperation(code, lhs, rhs) =>
          code shouldBe "FETCH_OBJ_R"
          lhs shouldBe ByteCodePlaceIndicator("THIS")
          rhs shouldBe StringLiteral("test")
        case x => fail(s"unexpected op $x")
      }
    }

    "be able to parse INSTANCEOF CV0($instance) (static) (no-autoload) (silent) (exception)" in {
      val op = "INSTANCEOF CV0($instance) (static) (no-autoload) (silent) (exception)"
      val Parsed.Success(_, length) = parse(op, getOperation(_))
      length shouldBe op.length
    }
    "be able to parse FAST_CALL 0016" in {
      val op = "FAST_CALL 0016"
      val Parsed.Success(result, length) = parse(op, getOperation(_))
      length shouldBe op.length
      result.op shouldBe SingleValueOperation("FAST_CALL", IntegerLiteral(16))
    }
    "be able to parse FAST_RET T10 try-catch(0)" in {
      val op = "FAST_RET T10 try-catch(0)"
      val Parsed.Success(result, length) = parse(op, getOperation(_))
      length shouldBe op.length
      result.op shouldBe DualValueOperation("FAST_RET", Variable("T10", tmp = true), TryCatchLiteral(0))
    }
    "be able to parse IS_IDENTICAL CV0($a) string(SINGLEBACKSLASH)" in {
      val op = "IS_IDENTICAL CV0($a) string(\"" + encode("""\""") + "\")"
      val Parsed.Success(result, length) = parse(op, getOperation(_))
      length shouldBe op.length
      result.op shouldBe DualValueOperation("IS_IDENTICAL", Variable("a", tmp = false), StringLiteral("""\"""))
    }
  }

  "parser getInstruction" should {
    "be able to correctly parse OP_DATA int(42)" in {
      val instruction = "OP_DATA int(42)"
      val Parsed.Success(result, length) =
        parse(input = instruction, getInstruction(_))
      length shouldBe instruction.length
      result match {
        case Operation(op) =>
          op match {
            case SingleValueOperation(name, value) =>
              name shouldBe "OP_DATA"
              value match {
                case IntegerLiteral(value) =>
                  value shouldBe 42
                case x => fail(s"unexpected value $x")
              }
            case x => fail(s"unexpected op $x")
          }
        case x => fail(s"unexpected operation $x")
      }
    }
    "be able to parse V1 = NEW 1 string(\"Basic\")" in {
      val instruction = s"""V1 = NEW 1 string("${encode("Basic")}")"""
      val Parsed.Success(result, length) =
        parse(input = instruction, getInstruction(_))
      length shouldBe instruction.length
      result match {
        case Assignment(lhs, rhs) =>
          lhs match {
            case Variable(name, tmp, reference) =>
              name shouldBe "V1"
              tmp shouldBe true
              reference shouldBe true
          }
          rhs match {
            case DualValueOperation(code, lhs, rhs) =>
              code shouldBe "NEW"
              lhs match {
                case IntegerLiteral(value) => value shouldBe 1
                case _ => fail(message = "lhs is not of type IntegerLiteral")
              }
              rhs match {
                case StringLiteral(value) => value shouldBe "Basic"
                case _ => fail(message = "rhs is not of type StringLiteral")
              }
            case x => fail(s"unexpected operation $x")
          }
        case _ => fail(message = "result is not of type Assignment")
      }
    }
    "be able to parse INIT_NS_FCALL_BY_NAME 1 string(\"PleskX\\Api\\get_class\")" in {
      val operation =
        s"""INIT_NS_FCALL_BY_NAME 1 string("${encode("PleskX\\Api\\get_class")}")"""
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result match {
        case op: Operation =>
          op.op match {
            case INIT_NS_FCALL_BY_NAME(paramCount, function) =>
              paramCount shouldBe 1
              function shouldBe "pleskx\\api\\get_class"
            case _ =>
              fail(message = "return is not of type INIT_NS_FCALL_BY_NAME")
          }
        case x => fail(s"unexpected operation $x")
      }
    }
    "be able to parse INCLUDE_OR_EVAL (require_once) string(\"../class.phpmailer.php\")" in {
      val operation =
        s"""INCLUDE_OR_EVAL (require_once) string("${encode("../class.phpmailer.php")}")"""
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result match {
        case op: Operation =>
          op.op match {
            case DualValueOperation(code, lhs, rhs) =>
              code shouldBe "INCLUDE_OR_EVAL"
              lhs shouldBe ByteCodeKeyword("require_once")
              rhs shouldBe StringLiteral("../class.phpmailer.php")
            case x => fail(s"unexpected op $x")
          }
        case x => fail(s"unexpected operation $x")
      }
    }
    "be able to parse FETCH_OBJ_R THIS string(\"Mail\")" in {
      val operation = s"""FETCH_OBJ_R THIS string("${encode("Mail")}")"""
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result match {
        case op: Operation =>
          op.op match {
            case DualValueOperation(code, lhs, rhs) =>
              code shouldBe "FETCH_OBJ_R"
              lhs shouldBe ByteCodePlaceIndicator("THIS")
              rhs shouldBe StringLiteral("Mail")
            case x => fail(s"unexpected op $x")
          }
        case x => fail(s"unexpected operation $x")
      }
    }
    "be able to parse SWITCH_STRING CV2($sType) \"to\": 0013" in {
      val operation = s"""SWITCH_STRING CV2($$sType) "${encode("to")}": 0013"""
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result match {
        case op: Operation =>
          op.op match {
            case SWITCH(code, variable, switches) =>
              code shouldBe "SWITCH_STRING"
              variable shouldBe Variable("sType", tmp = false)
              switches shouldBe Seq(("to", 13))
            case x => fail(s"unexpected op $x")
          }
        case x => fail(s"unexpected operation $x")
      }
    }
    "be able to parse SWITCH_STRING CV2($sType) \"to\": BB3" in {
      val operation = s"""SWITCH_STRING CV2($$sType) "${encode("to")}": BB3"""
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result match {
        case op: Operation =>
          op.op match {
            case SWITCH(code, variable, switches) =>
              code shouldBe "SWITCH_STRING"
              variable shouldBe Variable("sType", tmp = false)
              switches shouldBe Seq(("to", 3))
            case x => fail(s"unexpected op $x")
          }
        case x => fail(s"unexpected operation $x")
      }
    }
    "be able to parse SWITCH_STRING CV2($sType) \"to\": 0013, \"bcc\": 0014" in {
      val operation = s"""SWITCH_STRING CV2($$sType) "${encode("to")}": 0013, "${encode("bcc")}": 0014"""
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result match {
        case op: Operation =>
          op.op match {
            case SWITCH(code, variable, switches) =>
              code shouldBe "SWITCH_STRING"
              variable shouldBe Variable("sType", tmp = false)
              switches shouldBe Seq(("to", 13), ("bcc", 14))
            case x => fail(s"unexpected op $x")
          }
        case x => fail(s"unexpected operation $x")
      }
    }
    "be able to parse SWITCH_STRING string(\"cl\") \"cgi\": 0051, \"cgi-fcgi\": 0051, default: 0069" in {
      val operation = s"""SWITCH_STRING string("${encode("cl")}") "${encode("cgi")}": 0051, "${encode("cgi-fcgi")}": 0051, default: 0069"""
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result match {
        case op: Operation =>
          op.op match {
            case SWITCH(code, value, switches) =>
              code shouldBe "SWITCH_STRING"
              value shouldBe StringLiteral("cl")
              switches shouldBe Seq(("cgi", 51), ("cgi-fcgi", 51), ("default", 69))
            case x => fail(s"unexpected op $x")
          }
        case x => fail(s"unexpected operation $x")
      }
    }
    "be able to parse SWITCH_STRING CV2($sType) \"to\": 0013, \"bcc\": 0014, default: 0016" in {
      val operation =
        s"""SWITCH_STRING CV2($$sType) "${encode("to")}": 0013, "${encode("bcc")}": 0014, default: 0016"""
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result match {
        case op: Operation =>
          op.op match {
            case SWITCH(code, variable, switches) =>
              code shouldBe "SWITCH_STRING"
              variable shouldBe Variable("sType", tmp = false)
              switches shouldBe Seq(("to", 13), ("bcc", 14), ("default", 16))
            case x => fail(s"unexpected op $x")
          }
        case x => fail(s"unexpected operation $x")
      }
    }
    "be able to parse SWITCH_LONG CV($test) 33: 0013, 44: 0014, default: 0023" in {
      val operation = "SWITCH_LONG CV($test) 33: 0013, 44: 0014, default: 0023"
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result match {
        case op: Operation =>
          op.op match {
            case SWITCH(code, variable, switches) =>
              code shouldBe "SWITCH_LONG"
              variable shouldBe Variable("test", tmp = false)
              switches shouldBe Seq(("33", 13), ("44", 14), ("default", 23))
            case x => fail(s"unexpected op $x")
          }
        case x => fail(s"unexpected operation $x")
      }
    }
    "be able to parse RETURN_BY_REF (value) bool(false)" in {
      val operation = "RETURN_BY_REF (value) bool(false)"
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result match {
        case op: Operation =>
          op.op shouldBe DualValueOperation("RETURN_BY_REF", ByteCodeKeyword("value"), BooleanLiteral(false))
        case x => fail(s"unexpected operation $x")
      }
    }
    "be able to parse RETURN_BY_REF null" in {
      val operation = "RETURN_BY_REF null"
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result match {
        case op: Operation =>
          op.op shouldBe SingleValueOperation("RETURN_BY_REF", Null())
        case x => fail(s"unexpected operation $x")
      }
    }
    "be able to parse ECHO of multi line string" in {
      val encoded = encode(
        """
          |
          |Joomla! derives from copyrighted works licensed under the GNU General
          |Public License.  This version has been modified pursuant to the
          |GNU General Public License as of September 15, 2005, and as distributed,
          |it includes or is derivative of works licensed under the GNU General
          |Public License or other free or open source software licenses.  Please
          |see the CREDITS.php for a non-exhaustive list of contributors and
          |copyright holders.  A full text version of the GNU GPL version 2 can be
          |found in the LICENSE.php file.  A full text version of the other licenses
          |that Joomla! is derivative of or includes can be found in LICENSES.php.
          |
          |""".stripMargin)
      val operation =
        s"""ECHO string("$encoded")""".stripMargin
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result match {
        case op: Operation =>
          op.op match {
            case SingleValueOperation(code, value) =>
              code shouldBe "ECHO"
              value shouldBe StringLiteral(
                """
                  |
                  |Joomla! derives from copyrighted works licensed under the GNU General
                  |Public License.  This version has been modified pursuant to the
                  |GNU General Public License as of September 15, 2005, and as distributed,
                  |it includes or is derivative of works licensed under the GNU General
                  |Public License or other free or open source software licenses.  Please
                  |see the CREDITS.php for a non-exhaustive list of contributors and
                  |copyright holders.  A full text version of the GNU GPL version 2 can be
                  |found in the LICENSE.php file.  A full text version of the other licenses
                  |that Joomla! is derivative of or includes can be found in LICENSES.php.
                  |
                  |""".stripMargin)
            case x => fail(s"unexpected op $x")
          }
        case x => fail(s"unexpected operation $x")
      }
    }
    "be able to parse FETCH_OBJ_IS CV1($display) string(\"panels\")" in {
      val operation = s"""FETCH_OBJ_IS CV1($$display) string("${encode("panels")}")"""
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result match {
        case op: Operation =>
          op.op match {
            case DualValueOperation(code, lhs, rhs) =>
              code shouldBe "FETCH_OBJ_IS"
              lhs shouldBe Variable("display", tmp = false)
              rhs shouldBe StringLiteral("panels")
            case x => fail(s"unexpected op $x")
          }
        case x => fail(s"unexpected operation $x")
      }
    }
    "be able to parse IN_ARRAY 0 CV0($type) array(...)" in {
      val operation = "IN_ARRAY 0 CV0($type) array(...)"
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result match {
        case op: Operation =>
          op.op match {
            case TripleValueOperation(code, first, second, third) =>
              code shouldBe "IN_ARRAY"
              first shouldBe IntegerLiteral(0)
              second shouldBe Variable("type", tmp = false)
              third shouldBe ArrayValue(None)
            case x => fail(s"unexpected op $x")
          }
        case x => fail(s"unexpected operation $x")
      }
    }
    "be able to parse INIT_ARRAY 2 (packed) (ref) V11 NEXT" in {
      val operation = "INIT_ARRAY 2 (packed) (ref) V11 NEXT"
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result match {
        case op: Operation =>
          op.op match {
            case QuintupleValueOperation(code,
            first,
            second,
            third,
            fourth,
            fifth) =>
              code shouldBe "INIT_ARRAY"
              first shouldBe IntegerLiteral(2)
              second shouldBe ByteCodeKeyword("packed")
              third shouldBe ByteCodeKeyword("ref")
              fourth shouldBe Variable("V11", tmp = true, reference = true)
              fifth shouldBe ByteCodePlaceIndicator("NEXT")
            case x => fail(s"unexpected op $x")
          }
        case x => fail(s"unexpected operation $x")
      }
    }
    "be able to parse SEND_VAR_NO_REF_EX V30 1" in {
      val operation = "SEND_VAR_NO_REF_EX V30 1"
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result match {
        case op: Operation =>
          op.op match {
            case DualValueOperation(code, lhs, rhs) =>
              code shouldBe "SEND_VAR_NO_REF_EX"
              lhs shouldBe Variable("V30", tmp = true, reference = true)
              rhs shouldBe IntegerLiteral(1)
            case x => fail(s"unexpected op $x")
          }
        case x => fail(s"unexpected operation $x")
      }
    }
    "be able to parse CATCH string(\"Exception\")" in {
      val operation = s"""CATCH string("${encode("Exception")}")"""
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result match {
        case op: Operation =>
          op.op match {
            case SingleValueOperation(code, value) =>
              code shouldBe "CATCH"
              value shouldBe StringLiteral("Exception")
            case x => fail(s"unexpected op $x")
          }
        case x => fail(s"unexpected operation $x")
      }
    }
    "be able to parse CATCH string(\"Exception\") 0066" in {
      val operation = s"""CATCH string("${encode("Exception")}") 0066"""
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result match {
        case op: Operation =>
          op.op match {
            case DualValueOperation(code, lhs, rhs) =>
              code shouldBe "CATCH"
              lhs shouldBe StringLiteral("Exception")
              rhs shouldBe IntegerLiteral(66)
            case x => fail(s"unexpected op $x")
          }
        case x => fail(s"unexpected operation $x")
      }
    }
    "be able to parse ASSIGN_OBJ THIS string(\"auth_username\")" in {
      val operation = s"""ASSIGN_OBJ THIS string("${encode("auth_username")}")"""
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result match {
        case op: Operation =>
          op.op match {
            case DualValueOperation(code, lhs, rhs) =>
              code shouldBe "ASSIGN_OBJ"
              lhs shouldBe ByteCodePlaceIndicator("THIS")
              rhs shouldBe StringLiteral("auth_username")
            case x => fail(s"unexpected op $x")
          }
        case x => fail(s"unexpected operation $x")
      }
    }
    "be able to parse ASSIGN_OBJ_OP (CONCAT) THIS string(\"response\")" in {
      val operation = s"""ASSIGN_OBJ_OP (CONCAT) THIS string("${encode("response")}")"""
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result match {
        case op: Operation =>
          op.op match {
            case TripleValueOperation(code, first, second, third) =>
              code shouldBe "ASSIGN_OBJ_OP"
              first shouldBe AssignOpLiteral("CONCAT")
              second shouldBe ByteCodePlaceIndicator("THIS")
              third shouldBe StringLiteral("response")
            case x => fail(s"unexpected op $x")
          }
        case x => fail(s"unexpected operation $x")
      }
    }
    "be able to parse FE_FETCH_RW V6 CV2($frameLine) 0029" in {
      val operation = "FE_FETCH_RW V6 CV2($frameLine) 0029"
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result match {
        case op: Operation =>
          op.op match {
            case TripleValueOperation(code, first, second, third) =>
              code shouldBe "FE_FETCH_RW"
              first shouldBe Variable("V6", tmp = true, reference = true)
              second shouldBe Variable("frameLine", tmp = false)
              third shouldBe IntegerLiteral(29)
            case x => fail(s"unexpected op $x")
          }
        case x => fail(s"unexpected operation $x")
      }
    }
    "be able to parse SEND_VAL string(\"\") 2" in {
      val operation = "SEND_VAL string(\"\") 2"
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result match {
        case op: Operation =>
          op.op match {
            case DualValueOperation(code, lhs, rhs) =>
              code shouldBe "SEND_VAL"
              lhs shouldBe StringLiteral("")
              rhs shouldBe IntegerLiteral(2)
            case x => fail(s"unexpected op $x")
          }
        case x => fail(s"unexpected operation $x")
      }
    }
    "be able to parse ASSIGN_REF (function) CV0($backref_view) V2" in {
      val operation = "ASSIGN_REF (function) CV0($backref_view) V2"
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result match {
        case op: Operation =>
          op.op match {
            case TripleValueOperation(code, first, second, third) =>
              code shouldBe "ASSIGN_REF_3"
              first shouldBe ByteCodeKeyword("function")
              second shouldBe Variable("backref_view", tmp = false)
              third shouldBe Variable("V2", tmp = true, reference = true)
            case x => fail(s"unexpected op $x")
          }
        case x => fail(s"unexpected operation $x")
      }
    }
    "be able to parse RETURN null" in {
      val operation = "RETURN null"
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result match {
        case op: Operation =>
          op.op match {
            case SingleValueOperation(code, value) =>
              code shouldBe "RETURN"
              value shouldBe Null()
            case x => fail(s"unexpected op $x")
          }
        case x => fail(s"unexpected operation $x")
      }
    }
    "be able to parse RETURN_BY_REF (function) null" in {
      val operation = "RETURN_BY_REF (function) null"
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result match {
        case op: Operation =>
          op.op match {
            case DualValueOperation(code, lhs, rhs) =>
              code shouldBe "RETURN_BY_REF"
              lhs shouldBe ByteCodeKeyword("function")
              rhs shouldBe Null()
            case x => fail(s"unexpected op $x")
          }
        case x => fail(s"unexpected operation $x")
      }
    }
    "be able to parse ADD_ARRAY_ELEMENT array(...) string(\"libraries\")" in {
      val operation = s"""ADD_ARRAY_ELEMENT array(...) string("${encode("libraries")}")"""
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result match {
        case op: Operation =>
          op.op match {
            case DualValueOperation(code, lhs, rhs) =>
              code shouldBe "ADD_ARRAY_ELEMENT"
              lhs shouldBe ArrayValue(None)
              rhs shouldBe StringLiteral("libraries")
            case x => fail(s"unexpected op $x")
          }
        case x => fail(s"unexpected operation $x")
      }
    }
    "be able to parse FE_RESET_R array(...) 0267" in {
      val operation = "FE_RESET_R array(...) 0267"
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result match {
        case op: Operation =>
          op.op match {
            case DualValueOperation(code, lhs, rhs) =>
              code shouldBe "FE_RESET_R"
              lhs shouldBe ArrayValue(None)
              rhs shouldBe IntegerLiteral(267)
            case x => fail(s"unexpected op $x")
          }
        case x => fail(s"unexpected operation $x")
      }
    }
    "be able to parse ASSIGN_OBJ_REF (function) CV6($cache) string(\"display\")" in {
      val operation =
        s"""ASSIGN_OBJ_REF (function) CV6($$cache) string("${encode("display")}")"""
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result match {
        case op: Operation =>
          op.op match {
            case TripleValueOperation(code, first, second, third) =>
              code shouldBe "ASSIGN_OBJ_REF_3"
              first shouldBe ByteCodeKeyword("function")
              second shouldBe Variable("cache", tmp = false)
              third shouldBe StringLiteral("display")
            case x => fail(s"unexpected op $x")
          }
        case x => fail(s"unexpected operation $x")
      }
    }
    "be able to parse FETCH_OBJ_W CV0($account) string(\"content\")" in {
      val operation = s"""FETCH_OBJ_W CV0($$account) string("${encode("content")}")"""
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result match {
        case op: Operation =>
          op.op match {
            case DualValueOperation(code, lhs, rhs) =>
              code shouldBe "FETCH_OBJ_W_2"
              lhs shouldBe Variable("account", tmp = false)
              rhs shouldBe StringLiteral("content")
            case x => fail(s"unexpected op $x")
          }
        case x => fail(s"unexpected operation $x")
      }
    }
    "be bale to parse FETCH_OBJ_W (ref) V3 string(\"content\")" in {
      val operation = s"""FETCH_OBJ_W (ref) V3 string("${encode("content")}")"""
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result match {
        case op: Operation =>
          op.op match {
            case TripleValueOperation(code, first, second, third) =>
              code shouldBe "FETCH_OBJ_W_3"
              first shouldBe ByteCodeKeyword("ref")
              second shouldBe Variable("V3", tmp = true, reference = true)
              third shouldBe StringLiteral("content")
            case x => fail(s"unexpected op $x")
          }
        case x => fail(s"unexpected operation $x")
      }
    }
    "be able to parse ASSIGN_STATIC_PROP string(\"leeway\") string(\"Firebase\\JWT\\JWT\")" in {
      val operation =
        s"""ASSIGN_STATIC_PROP string("${encode("leeway")}") string("${encode("Firebase\\JWT\\JWT")}")"""
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result match {
        case op: Operation =>
          op.op match {
            case DualValueOperation(code, lhs, rhs) =>
              code shouldBe "ASSIGN_STATIC_PROP_2"
              lhs shouldBe StringLiteral("leeway")
              rhs shouldBe StringLiteral("Firebase\\JWT\\JWT")
            case x => fail(s"unexpected op $x")
          }
        case x => fail(s"unexpected operation $x")
      }
    }
    "be able to parse TICKS 1" in {
      val operation = "TICKS 1"
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result shouldBe Operation(SingleValueOperation("TICKS", IntegerLiteral(1)))
    }
    "be able to parse ADD_ARRAY_ELEMENT float(inf) string(\"depth_min_leaf\")" in {
      val operation = s"""ADD_ARRAY_ELEMENT float(inf) string("${encode("depth_min_leaf")}")"""
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result shouldBe Operation(
        DualValueOperation("ADD_ARRAY_ELEMENT", FloatLiteral(Float.PositiveInfinity), StringLiteral("depth_min_leaf")))
    }
    "be able to parse ASSIGN_STATIC_PROP_OP (CONCAT) string(\"extraHeaderHTML\")" in {
      val operation = s"""ASSIGN_STATIC_PROP_OP (CONCAT) string("${encode("extraHeaderHTML")}")"""
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result shouldBe Operation(
        DualValueOperation("ASSIGN_STATIC_PROP_OP", AssignOpLiteral("CONCAT"), StringLiteral("extraHeaderHTML"))
      )
    }
    "be able to parse FUNC_GET_ARGS" in {
      val operation = "FUNC_GET_ARGS"
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result shouldBe Operation(
        NoValueOperation("FUNC_GET_ARGS")
      )
    }
    "be able to parse FUNC_GET_ARGS int(1)" in {
      val operation = "FUNC_GET_ARGS int(1)"
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result shouldBe Operation(
        SingleValueOperation("FUNC_GET_ARGS", IntegerLiteral(1))
      )
    }
    "be able to parse POST_INC_STATIC_PROP string(\"writes\")" in {
      val operation = s"""POST_INC_STATIC_PROP string("${encode("writes")}")"""
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result shouldBe Operation(
        SingleValueOperation("POST_INC_STATIC_PROP", StringLiteral("writes"))
      )
    }
    "be able to parse INIT_FCALL 1 96 string(\"mb_strlen\")" in {
      val operation = s"""INIT_FCALL 1 96 string("${encode("mb_strlen")}")" + otherStuff"""
      val Parsed.Success(result, _) = parse(operation, getInstruction(_))
      result shouldBe Operation(
        INIT_FCALL(1, 96, StringLiteral("mb_strlen"))
      )
    }
    "be able to parse  SEND_USER string(\"spc\") 3" in {
      val operation = s"""SEND_USER string("${encode("spc")}") 3"""
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result shouldBe Operation(
        DualValueOperation("SEND_USER", StringLiteral("spc"), IntegerLiteral(3))
      )
    }
    "be able to parse PRE_INC_STATIC_PROP string(\"value\") string(\"other\")" in {
      val operation = s"""PRE_INC_STATIC_PROP string("${encode("value")}") string("${encode("other")}")"""
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result shouldBe Operation(
        DualValueOperation("PRE_INC_STATIC_PROP", StringLiteral("value"), StringLiteral("other"))
      )
    }
    "be able to parse FETCH_STATIC_PROP_IS string(\"documents\") string(\"phpQuery\")" in {
      val operation = s"""FETCH_STATIC_PROP_IS string("${encode("documents")}") string("${encode("phpQuery")}")"""
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result shouldBe Operation(
        DualValueOperation("FETCH_STATIC_PROP_IS", StringLiteral("documents"), StringLiteral("phpQuery"))
      )
    }
    "be able to parse DECLARE_ANON_CLASS string(\"class@anonymous\") string(\"vanilla\\models\\model\")" in {
      val operation = s"""DECLARE_ANON_CLASS string("${encode("class@anonymous")}") string("${encode("vanilla\\models\\model")}")"""
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result shouldBe Operation(
        DualValueOperation("DECLARE_ANON_CLASS", StringLiteral("class@anonymous"), StringLiteral("vanilla\\models\\model"))
      )
    }
    "be able to process weird string stuff" in {
      val operation = s"""ADD_ARRAY_ELEMENT string("${encode("$G")}") string("${encode("^G")}")"""
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result shouldBe Operation(
        DualValueOperation("ADD_ARRAY_ELEMENT", StringLiteral("$G"), StringLiteral("^G"))
      )
    }
    "be able to parse FETCH_OBJ_R THIS string(\"b\")" in {
      val operation = s"""FETCH_OBJ_R THIS string("${encode("b")}")"""
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result shouldBe Operation(
        DualValueOperation("FETCH_OBJ_R", ByteCodePlaceIndicator("THIS"), StringLiteral("b"))
      )
    }
    "be able to parse PRE_DEC_STATIC_PROP string(\"active\")" in {
      val operation = s"""PRE_DEC_STATIC_PROP string("${encode("active")}")"""
      val Parsed.Success(result, length) = parse(operation, getInstruction(_))
      length shouldBe operation.length
      result shouldBe Operation(
        SingleValueOperation("PRE_DEC_STATIC_PROP", StringLiteral("active"))
      )
    }
    "be able to parse FETCH_GLOBALS" in {
      val op = "FETCH_GLOBALS"
      val Parsed.Success(result, length) = parse(op, getInstruction(_))
      length shouldBe op.length
      result shouldBe Operation(
        NoValueOperation("FETCH_GLOBALS")
      )
    }
    "be able to parse VERIFY_NEVER_TYPE" in {
      val op = "VERIFY_NEVER_TYPE"
      val Parsed.Success(result, length) = parse(op, getInstruction(_))
      length shouldBe op.length
      result shouldBe Operation(
        NoValueOperation("VERIFY_NEVER_TYPE")
      )
    }
    "be able to parse CALLABLE_CONVERT" in {
      val op = "CALLABLE_CONVERT"
      val Parsed.Success(result, length) = parse(op, getInstruction(_))
      length shouldBe op.length
      result shouldBe Operation(
        NoValueOperation("CALLABLE_CONVERT")
      )
    }
    "be able to parse CASE_STRICT T1 int(2)" in {
      val op = "CASE_STRICT T1 int(2)"
      val Parsed.Success(result, length) = parse(op, getInstruction(_))
      length shouldBe op.length
      result shouldBe Operation(DualValueOperation("CASE_STRICT", Variable("T1", tmp = true), IntegerLiteral(2)))
    }
    "be able to parse DECLARE_LAMBDA_FUNCTION 0" in {
      val op = "DECLARE_LAMBDA_FUNCTION 0"
      val Parsed.Success(result, length) = parse(op, getInstruction(_))
      length shouldBe op.length
      result shouldBe Operation(SingleValueOperation("DECLARE_LAMBDA_FUNCTION", IntegerLiteral(0)))
    }
    "be able to parse TYPE_CHECK (false) V2" in {
      val op = "TYPE_CHECK (false) V2"
      val Parsed.Success(result, length) = parse(op, getInstruction(_))
      length shouldBe op.length
      result shouldBe Operation(DualValueOperation("TYPE_CHECK", BooleanLiteral(false), Variable("V2", tmp = true, reference = true)))
    }
    "be able to parse TYPE_CHECK TYPE [bool, long, double, string, array, object, resource] T6" in {
      val op = "TYPE_CHECK TYPE [bool, long, double, string, array, object, resource] T6"
      val Parsed.Success(result, length) = parse(op, getInstruction(_))
      length shouldBe op.length
      result shouldBe Operation(DualValueOperation("TYPE_CHECK",
        StringLiteral("TYPE [bool, long, double, string, array, object, resource]"),
        Variable("T6", tmp = true)))
    }
    "be able to parse CHECK_VAR" in {
      val string = "CHECK_VAR CV0($undef)"
      val Parsed.Success(result, length) = parse(string, getOperation(_))
      length shouldBe string.length
      result.op match {
        case SingleValueOperation(code, value) => code shouldBe "CHECK_VAR"
          value shouldBe Variable("undef", tmp = false)
        case _ => fail()
      }
    }
  }

  "be able to parse named parameters" should {
    "be able to parse SEND_VAL[_X] bool(false) string(\"double_encode\")" in {
      // I've only seen SEND_VAL in the wild so far, the rest is extrapolated. /Malte
      for (x <- Seq("SEND_VAL", "SEND_VAL_EX", "SEND_USER", "SEND_REF")) {
        val operation = x + s""" bool(false) string("${encode("double_encode")}")"""
        val Parsed.Success(result, length) = parse(operation, getInstruction(_), verboseFailures = true)
        length shouldBe operation.length
        result match {
          case op: Operation =>
            op.op match {
              case DualValueOperation(code, lhs, rhs) =>
                code shouldBe x
                lhs shouldBe BooleanLiteral(false)
                rhs shouldBe StringLiteral("double_encode")
              case x => fail(s"unexpected operation $x")
            }
          case x => fail(s"unexpected operation $x")
        }
      }
    }
    "be able to parse SEND_VAR[_X] bool(false) string(\"double_encode\")" in {
      for (x <- Seq("SEND_VAR", "SEND_VAR_EX", "SEND_VAR_NO_REF_EX", "SEND_VAR_NO_REF", "SEND_FUNC_ARG")) {
        val operation = x + s""" CV($$a) string("${encode("double_encode")}")"""
        val Parsed.Success(result, length) = parse(operation, getInstruction(_), verboseFailures = true)
        length shouldBe operation.length
        result match {
          case op: Operation =>
            op.op match {
              case DualValueOperation(code, lhs, rhs) =>
                code shouldBe x
                lhs shouldBe Variable("a", tmp = false)
                rhs shouldBe StringLiteral("double_encode")
              case x => fail(s"unexpected operation $x")
            }
          case x => fail(s"unexpected operation $x")
        }
      }
    }
  }

  "be able to parse COPY_TMP T2" in {
    val operation = "COPY_TMP T2"
    val Parsed.Success(result, length) = parse(operation, parseOperation(_), verboseFailures = true)
    length shouldBe operation.length
    result match {
      case SingleValueOperation(code, value) =>
        code shouldBe "COPY_TMP"
        value shouldBe Variable("T2", tmp = true)
    }
  }

  "be able to parse match instructions" should {
    "complicated usage with fake default" in {
      val operation = s"""MATCH CV0($$food) "${encode("apple")}": BB2, "${encode("bar")}": BB3, "${encode("default")}": BB4, "${encode("apple2")}": BB5, "${encode("bar2")}": BB6, "${encode("cake2")}": BB7, default: BB1"""
      val Parsed.Success(result, length) = parse(operation, parseOperation(_))
      length shouldBe operation.length
      val rc = result.asInstanceOf[MatchOpcode]
      rc.code shouldBe "MATCH"
      rc.default shouldBe "1"
      rc.values shouldBe Seq(
        KeyValuePair(Right("apple"), "2"),
        KeyValuePair(Right("bar"), "3"),
        KeyValuePair(Right("default"), "4"),
        KeyValuePair(Right("apple2"), "5"),
        KeyValuePair(Right("bar2"), "6"),
        KeyValuePair(Right("cake2"), "7"))
    }

    "int and string" in {
      val operation = s"""MATCH CV0($$food) 1: 0005, "${encode("42")}": 0007, default: 0004"""
      val Parsed.Success(result, length) = parse(operation, parseOperation(_), verboseFailures = true)
      length shouldBe operation.length
      val rc = result.asInstanceOf[MatchOpcode]
      rc.code shouldBe "MATCH"
      rc.default shouldBe "0004"
      rc.values shouldBe Seq(KeyValuePair(Left(1), "0005"),
        KeyValuePair(Right("42"), "0007"))
    }
  }

  "be able to parse const array initialization" when {
    "empty array" in {
      val operation =
        "ASSIGN CV0($x) array()"
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case DualValueOperation(code, lhs, rhs) =>
          code shouldBe "ASSIGN"
          lhs match {
            case Variable(name, tmp, reference) =>
              name shouldBe "x"
              tmp shouldBe false
              reference shouldBe false
          }
          rhs match {
            case ArrayValue(content) => content shouldBe Some(List())
          }
      }
    }

    "named and index values" in {
      val operation =
        "ASSIGN CV0($x) array(N:string(\"Zm9v\") string(\"YmFy\")|P:int(0) int(1)|P:int(1) int(2)|P:int(2) int(3)|)"
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case DualValueOperation(code, lhs, rhs) =>
          code shouldBe "ASSIGN"
          lhs match {
            case Variable(name, tmp, reference) =>
              name shouldBe "x"
              tmp shouldBe false
              reference shouldBe false
            case _ => fail(message = "lhs is not of type Variable")
          }
          rhs match {
            case ArrayValue(arr) => arr match {
              case Some(value) => value shouldBe List(
                ArrayKeyValuePair(Right("foo"), StringLiteral("bar")),
                ArrayKeyValuePair(Left(0), IntegerLiteral(1)),
                ArrayKeyValuePair(Left(1), IntegerLiteral(2)),
                ArrayKeyValuePair(Left(2), IntegerLiteral(3)),
              )
              case None => fail("array init missing")
            }
            case _ => fail(message = "rhs is not of type ArrayValue")
          }
        case _ => fail(message = "result is not of type DualValueOperation")
      }
    }
    "nested arrays" in {
      val operation = "ASSIGN CV0($x) array(P:int(0) array(...)|P:int(1) string(\"Zm9vYmFy\")|)"
      val Parsed.Success(result, length) = parse(operation, getOperation(_))
      length shouldBe operation.length
      result.op match {
        case DualValueOperation(code, lhs, rhs) =>
          code shouldBe "ASSIGN"
          lhs match {
            case Variable(name, tmp, reference) =>
              name shouldBe "x"
              tmp shouldBe false
              reference shouldBe false
            case _ => fail(message = "lhs is not of type Variable")
          }
          rhs match {
            case ArrayValue(arr) => arr match {
              case Some(value) => value shouldBe List(
                ArrayKeyValuePair(Left(0), ArrayValue(None)),
                ArrayKeyValuePair(Left(1), StringLiteral("foobar")),
              )
              case None => fail("array init missing")
            }
            case _ => fail(message = "rhs is not of type ArrayValue")
          }
        case _ => fail(message = "result is not of type DualValueOperation")
      }
    }
  }

  "parseDestinationPattern" should {
    "parse \"to\": 0013" in {
      val string = s""""${encode("to")}": 0013"""
      val Parsed.Success(result, length) =
        parse(string, parseStringDestinationPattern(_))
      length shouldBe string.length
      result shouldBe (("to", "0013"))
    }
    "parse default: 0013" in {
      val string = "default: 0013"
      val Parsed.Success(result, length) =
        parse(string, parseStringDestinationPattern(_))
      length shouldBe string.length
      result shouldBe (("default", "0013"))
    }
    "parse 44: 0013" in {
      val string = "44: 0013"
      val Parsed.Success(result, length) =
        parse(string, parseNumberDestinationPattern(_))
      length shouldBe string.length
      result shouldBe (("44", "0013"))
    }
    "parse default: 0016" in {
      val string = "default: 0016"
      val Parsed.Success(result, length) =
        parse(string, parseNumberDestinationPattern(_))
      length shouldBe string.length
      result shouldBe (("default", "0016"))
    }
  }
}
