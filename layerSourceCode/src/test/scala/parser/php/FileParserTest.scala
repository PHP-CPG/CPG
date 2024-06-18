package parser.php

import io.joern.TraversalUtils
import io.joern.php.CpgTestFixture
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class FileParserTest extends AnyWordSpec with Matchers with TraversalUtils {

  val fixture: CpgTestFixture = CpgTestFixture("onlyMainCreation")

  "project" should {
    "have a single file" in {
      fixture.cpg.file.l.length shouldBe 1
    }
    "have a single namespaceblock" in {
      fixture.cpg.namespaceBlock.l.length shouldBe 1
    }
    "that have 1 phpinfo call" in {
      fixture.cpg.call.l.length shouldBe 3
      fixture.cpg.call("phpinfo").l.length shouldBe 1
    }
  }
}
