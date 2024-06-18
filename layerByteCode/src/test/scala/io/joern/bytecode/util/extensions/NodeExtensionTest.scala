package io.joern.bytecode.util.extensions

import io.joern.bytecode.parser
import io.joern.bytecode.parser.PHPVersion._
import io.joern.bytecode.parser.PHPVersions
import io.joern.bytecode.util.extensions.NodeExtension._
import io.joern.bytecode.util.unittesting.CpgFromCodeTestFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier, Literal}
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class NodeExtensionTest extends AnyWordSpec with Matchers with PHPVersions {

  for(v <- getPhpVersions) {
    implicit val version: parser.PHPVersion.Value = v
    s"call node extension in PHP $version" should {
      "give me the right arguments" in new CpgFromCodeTestFixture(
        """
          |f(1,$eins);
          |""".stripMargin) {
        val call: List[Call] = cpg.call.nameExact("f").l
        call.length shouldBe 1
        call.head.getParameter(1).get.asInstanceOf[Literal].code shouldBe "1"
        call.head.getParameter(2).get.asInstanceOf[Identifier].name shouldBe "eins"
      }
      "deal with call_user_func_array" in new CpgFromCodeTestFixture(
        """call_user_func_array('file_get_contents', $args);"""
      ) {
        val call = cpg.call.nameExact("file_get_contents").l
        call.length shouldBe 1
        call.head.getParameter(0).get.asInstanceOf[Identifier].name shouldBe "args"
      }
    }

  }

  {
    implicit val version: parser.PHPVersion.Value = V8
    s"call node extension PHP $version specific" should {
      "give me the right named parameter" in  new CpgFromCodeTestFixture(
        """
          |f(named : 1);
          |""".stripMargin) {
        val call: List[Call] = cpg.call.nameExact("f").l
        call.length shouldBe 1
        call.head.getParameter("named").get.asInstanceOf[Literal].code shouldBe "1"
      }
      "give me the right parameter even if mixed" in new CpgFromCodeTestFixture(
        """
          |f($var, 7, named : 42, other : 33);
          |""".stripMargin
      ) {
        val call: List[Call] = cpg.call.nameExact("f").l
        call.length shouldBe 1
        call.head.getParameter(1).get.asInstanceOf[Identifier].name shouldBe "var"
        call.head.getParameter("named").get.asInstanceOf[Literal].code shouldBe "42"
        call.head.getParameter("other").get.asInstanceOf[Literal].code shouldBe "33"
      }
    }
  }

}
