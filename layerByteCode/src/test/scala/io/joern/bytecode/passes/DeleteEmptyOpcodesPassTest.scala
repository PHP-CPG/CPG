package io.joern.bytecode.passes

import io.joern.bytecode.parser.{PHPVersion, PHPVersions}
import io.joern.bytecode.util.unittesting.CpgFromCodeTestFixture
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DeleteEmptyOpcodesPassTest extends AnyWordSpec with Matchers with PHPVersions {

  for(v <- getPhpVersions) {
    implicit val version: PHPVersion.Value = v

    s"cpg for PHP $version" should {

      "have no more EXT_STMT nodes in trivial cfg" in new CpgFromCodeTestFixture(
        "$x = 5; echo $x") {
        cpg.call("EXT_STMT").toList.length shouldBe 0
      }

      "have a no more EXT_STMT nodes in complex cfg" in new CpgFromCodeTestFixture(
        """
          |$x = 5;
          |if($x == 6) {
          |   $x += 4;
          |} else {
          |   $x += 5;
          |}
          |echo $x;
          |""".stripMargin) {
        cpg.call("EXT_STMT").toList.length shouldBe 0
      }
    }
  }
}
