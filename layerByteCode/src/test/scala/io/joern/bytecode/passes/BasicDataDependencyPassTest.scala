package io.joern.bytecode.passes

import io.joern.bytecode.parser
import io.joern.bytecode.parser.PHPVersions
import io.joern.bytecode.util.unittesting.CpgFromCodeTestFixture
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BasicDataDependencyPassTest extends AnyWordSpec with Matchers with PHPVersions  {

  for(v <- getPhpVersions) {
    implicit val version: parser.PHPVersion.Value = v

    s"cpg for PHP $version" should {
      "have a single ddg edge from the assignment" in new CpgFromCodeTestFixture(
        "$x = 5; echo $x;") {
        implicit val method: String = "dlr_main"
        ddgSuccOf("ASSIGN CV($x) int(5)") shouldBe expectedDdg(
          ("ECHO CV($x)", "x"))
      }

      "have a single ddg edge from the global declaration" in new CpgFromCodeTestFixture(
        """
          |function func() {
          |   global $x;
          |   echo $x;
          |}
          |""".stripMargin) {
        implicit val method: String = "func"
        ddgSuccOf(s"""BIND_GLOBAL CV($$x) string("x")""") shouldBe expectedDdg(
          ("ECHO CV($x)", "x"))
      }

      "have a single ddg edge from the assign op" in new CpgFromCodeTestFixture(
        """
          |function func() {
          |   $x += 5;
          |   echo $x;
          |}
          |""".stripMargin) {
        implicit val method: String = "func"
        ddgSuccOf("ASSIGN_OP (ADD) CV($x) int(5)") shouldBe expectedDdg(
          ("ECHO CV($x)", "x"))
      }

      "have a ddg edge between declaration and reassignment" in new CpgFromCodeTestFixture(
        "$x = 5; $x += 5;") {
        implicit val method: String = "dlr_main"
        ddgSuccOf("ASSIGN CV($x) int(5)") shouldBe expectedDdg(
          ("ASSIGN_OP (ADD) CV($x) int(5)", "x"))
      }

      "have two ddg edges when reassignment" in new CpgFromCodeTestFixture(
        "$x = 5; $x += 5; echo $x;") {
        implicit val method: String = "dlr_main"
        ddgSuccOf("ASSIGN CV($x) int(5)") shouldBe expectedDdg(
          ("ASSIGN_OP (ADD) CV($x) int(5)", "x"))
        ddgSuccOf("ASSIGN_OP (ADD) CV($x) int(5)") shouldBe expectedDdg(
          ("ECHO CV($x)", "x"))
      }

      "have a split of the ddg due to if" in new CpgFromCodeTestFixture(
        """
          |$x = 5;
          |if($x == 6) {
          |   $x += 4;
          |} else {
          |   $x += 5;
          |}
          |echo $x;
          |""".stripMargin) {
        implicit val method: String = "dlr_main"
        ddgSuccOf("ASSIGN CV($x) int(5)") shouldBe expectedDdg(
          ("IS_EQUAL CV($x) int(6)", "x"),
          ("ASSIGN_OP (ADD) CV($x) int(4)", "x"),
          ("ASSIGN_OP (ADD) CV($x) int(5)", "x"))
        ddgSuccOf("T2 = IS_EQUAL CV($x) int(6)") shouldBe
          expectedDdg(("JMPZ T2 int(5)", "T2"))
        ddgSuccOf("ASSIGN_OP (ADD) CV($x) int(4)") shouldBe expectedDdg(
          ("ECHO CV($x)", "x"))
        ddgSuccOf("ASSIGN_OP (ADD) CV($x) int(5)") shouldBe expectedDdg(
          ("ECHO CV($x)", "x"))
      }

      "be able to handle = assignment in code" in new CpgFromCodeTestFixture(
        """$statements = ["DELETE FROM `gui_group` WHERE `group_id` = $group_id",
          |      "UPDATE `gui_account` SET `account_group` = 0 WHERE `account_group` = $group_id",];
          |    foreach ($statements as $statement) {
          |      $db->exec($statement);
          |    }
          |""".stripMargin
      ) {
        implicit val method: String = "dlr_main"
        ddgSuccOf("T5 = ADD_ARRAY_ELEMENT T6 NEXT") shouldBe expectedDdg(("ASSIGN CV($statements) T5", "T5"))
      }
    }
  }
}
