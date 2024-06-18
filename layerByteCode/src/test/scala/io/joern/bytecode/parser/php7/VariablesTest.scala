package io.joern.bytecode.parser.php7

import fastparse._
import io.joern.bytecode.parser.php7.Variables._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class VariablesTest extends AnyWordSpec with Matchers {

  "parser getVariable" should {
    "be able to parse 'CV($variable_name)'" in {
      val Parsed.Success(result, _) =
        parse("CV($variable_name)", getVariable(_))
      assert(result.name == "variable_name")
      assert(result.tmp == false)
    }
  }

  "parser getTemporary" should {
    "should be able to parse 'T1'" in {
      val Parsed.Success(result, _) = parse("T1", getTemporary(_))
      assert(result.name == "T1")
      assert(result.tmp == true)
    }
  }

}
