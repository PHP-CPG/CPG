package io.joern.bytecode.passes

import io.joern.bytecode.parser.{PHPVersion, PHPVersions}
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import overflowdb.Node

import java.io.File
import scala.jdk.CollectionConverters._

class CpgConditionalTest extends AnyWordSpec with Matchers with PHPVersions  {

  for(v <- getPhpVersions) {
    implicit val version: PHPVersion.Value = v

    val fixture: CpgTestFixture = CpgTestFixture("basicConditional")

    def getSingleFile(name: String): Node = {
      fixture.cpg.file.nameExact(name).l match {
        case List(x) => x
        case _ => fail()
      }
    }

    def getMethods(name: String): List[Node] = {
      fixture.cpg.method.nameExact(name).l
    }


    s"CPG layout for PHP $version" should {
      "have a single file" in {
        fixture.files.length shouldBe 1
        getSingleFile(fixture.files.head.getPath)
      }
      "have the method main" in {
        getMethods("dlr_main")
      }
    }

    s"CFG of CPG for PHP $version" should {
      "have a distinct edge pattern" in {
        val method = fixture.cpg.method("dlr_main").l
        method.length shouldBe 1
        var next = method.head._cfgOut.next()._cfgOut.next()
        next.asInstanceOf[nodes.Identifier].name shouldBe "x"
        next = next._cfgOut.next()
        next.asInstanceOf[nodes.Literal].code shouldBe "42"
        next = next._cfgOut.next()
        next.asInstanceOf[nodes.Call].code shouldBe "ASSIGN CV($x) int(42)"
        next = next._cfgOut.next()
        next.asInstanceOf[nodes.Identifier].code shouldBe "CV($x)"
        next = next._cfgOut.next()
        next.asInstanceOf[nodes.Literal].code shouldBe "43"
        next = next._cfgOut.next()
        next.asInstanceOf[nodes.Call].code shouldBe "IS_EQUAL CV($x) int(43)"
        next = next._cfgOut.next()
        next.asInstanceOf[nodes.Identifier].code shouldBe "T2"
        next = next._cfgOut.next()
        next.asInstanceOf[nodes.Call].code shouldBe "T2 = IS_EQUAL CV($x) int(43)"
        next = next._cfgOut.next()
        next.asInstanceOf[nodes.Identifier].name shouldBe "T2"
        next = next._cfgOut.next()
        next.asInstanceOf[nodes.Literal].code shouldBe "5"
        next = next._cfgOut.next()
        next.asInstanceOf[nodes.Call].code shouldBe "JMPZ T2 int(5)"
        next._cfgOut.asScala.length shouldBe 2
      }
    }
  }
}
