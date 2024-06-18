package io.joern.bytecode.passes

import io.joern.bytecode.parser.{PHPVersion, PHPVersions}
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CpgParsingSideCasesTest
    extends AnyWordSpec
    with Matchers with PHPVersions {

  for(v <- getPhpVersions) {
    implicit val version: PHPVersion.Value = v

    val fixture: CpgTestFixture = CpgTestFixture("parsingSideCases")

    s"sidecases for PHP $version" should {
      "be able to parse the given sidecase assortment successfully" in {
        fixture.cpg.method("dlr_main").l.length shouldBe 1
      }
    }
  }
}
