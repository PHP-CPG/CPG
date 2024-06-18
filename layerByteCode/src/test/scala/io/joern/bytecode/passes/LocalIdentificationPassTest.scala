package io.joern.bytecode.passes

import io.joern.bytecode.parser.{PHPVersion, PHPVersions}
import io.joern.bytecode.util.unittesting.CpgFromCodeTestFixture
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class LocalIdentificationPassTest extends AnyWordSpec with Matchers with PHPVersions {

  for(v <- getPhpVersions) {
    implicit val version: PHPVersion.Value = v

    s"cpgStructure for PHP $version" should {
      "have no locals" in new CpgFromCodeTestFixture(
        """mysql_query("BEGIN");
          |mysql_query("UPDATE ttable SET test = '32' WHERE other = '33'");
          |mysql_query("UPDATE ttable SET test = '32' WHERE other = '33'");
          |mysql_query("COMMIT");
          |""".stripMargin
      ) {
        cpg.local.size shouldBe 0
      }

      "return two transaction issues for the same transaction" in new CpgFromCodeTestFixture(
        """$var = 42
          |""".stripMargin
      ) {
        cpg.local.toList match {
          case elem :: Nil => elem.name shouldBe "var"
          case x => fail(s"unexpected traversal result $x")
        }
      }
    }
  }

}
