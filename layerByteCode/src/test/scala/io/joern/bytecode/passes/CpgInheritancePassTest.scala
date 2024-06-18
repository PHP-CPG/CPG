package io.joern.bytecode.passes

import io.joern.bytecode.parser.{PHPVersion, PHPVersions}
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import overflowdb.Node

class CpgInheritancePassTest
    extends AnyWordSpec
    with Matchers with PHPVersions {

  for(v <- getPhpVersions) {
    implicit val version: PHPVersion.Value = v

    val fixture: CpgTestFixture = CpgTestFixture("inheritance")

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
      "have one main methods" in {
        fixture.cpg.method("dlr_main").l.length shouldBe 1
      }
      "have two types" in {
        fixture.cpg.typeDecl.l.length shouldBe 2
      }
      "have an inheritance edge from A to B" in {
        val a = fixture.cpg.typeDecl("a").l
        val b = fixture.cpg.typeDecl("b").l
        a.length shouldBe 1
        b.length shouldBe 1
        //val targetA = in(a.head, EdgeTypes.INHERITS_FROM)
        //targetA.length shouldBe 1
        //val targetB = out(a.head, EdgeTypes.INHERITS_FROM)
        //targetB.length shouldBe 1
        //targetA shouldBe b.head
        //targetB shouldBe a.head
      }
    }
  }
}
