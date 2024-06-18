package io.joern.bytecode.passes

import io.joern.bytecode.parser
import io.joern.bytecode.parser.PHPVersions
import io.joern.bytecode.util.unittesting.CpgFromCodeTestFixture
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DynamicCallsTest extends AnyWordSpec with Matchers with PHPVersions {

  for(v <- getPhpVersions) {
    implicit val version: parser.PHPVersion.Value = v

    s"cpg for PHP $version" should {

      "have a proper INIT_DYNAMIC_CALL node" in new CpgFromCodeTestFixture(
        """
          |$test();
          |""".stripMargin
      ) {
        cpg.call("INIT_DYNAMIC_CALL").hasNext shouldBe true
        cpg.call("INIT_DYNAMIC_CALL").next().argument.toList.length shouldBe 2
        cpg.call("INIT_DYNAMIC_CALL").next().code shouldBe "INIT_DYNAMIC_CALL 0 CV($test)"
      }

    }
  }

}
