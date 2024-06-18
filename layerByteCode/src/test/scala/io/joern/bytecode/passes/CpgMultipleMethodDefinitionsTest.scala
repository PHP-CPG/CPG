package io.joern.bytecode.passes

import io.joern.bytecode.parser.{PHPVersion, PHPVersions}
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import overflowdb.Node

class CpgMultipleMethodDefinitionsTest
    extends AnyWordSpec
    with Matchers with PHPVersions  {


  for(v <- getPhpVersions) {
    implicit val version: PHPVersion.Value = v

    val fixture: CpgTestFixture = CpgTestFixture("twoFunctionsAndMain")

    def getMethods(name: String): List[Node] = {
      fixture.cpg.method.nameExact(name).l
    }

    def getSingleFile(name: String): Node = {
      fixture.cpg.file.nameExact(name).l match {
        case List(x) => x
        case _ => fail()
      }
    }

    s"CFG layout for PHP $version" should {
      "have a single file" in {
        getSingleFile(fixture.files.head.getPath)
      }
      "have the method main" in {
        getMethods("DLR_main")
      }
      "have the method first_function" in {
        getMethods("first_function")
      }
      "have the method second_function" in {
        getMethods("second_function")
      }
    }
  }
}
