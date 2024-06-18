package io.joern.bytecode.passes

import io.joern.bytecode.parser.{PHPVersion, PHPVersions}
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import overflowdb.Node

class CpgMultipleFilesProjectTest
    extends AnyWordSpec
    with Matchers with PHPVersions {

  for(v <- getPhpVersions) {
    implicit val version: PHPVersion.Value = v

    val fixture: CpgTestFixture = CpgTestFixture("multipleFilesProject")

    def getSingleFile(name: String): Node = {
      fixture.cpg.file.nameExact(name).l match {
        case List(x) => x
        case _ => fail()
      }
    }

    s"CPG layout for PHP $version" should {
      "have two files" in {
        fixture.files.length shouldBe 2
        getSingleFile(fixture.files.head.getPath)
        getSingleFile(fixture.files(1).getPath)
      }
      "have two main methods" in {
        fixture.cpg.method.nameExact("dlr_main").l.length shouldBe 2
      }
    }
  }
}
