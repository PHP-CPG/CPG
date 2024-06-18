package io.joern.bytecode.passes

import io.joern.bytecode.parser.{PHPVersion, PHPVersions}
import io.joern.bytecode.util.unittesting.CpgFromCodeTestFixture
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{Method, MethodReturn}
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import overflowdb.traversal.jIteratortoTraversal

class CreateStubMethodPassTest  extends AnyWordSpec with Matchers with PHPVersions {

  for(v <- getPhpVersions) {

    implicit val version: PHPVersion.Value = v

    s"stub creation for PHP $version" should {
      s"be able to handle similarly named method of whom one class is unknown" in new CpgFromCodeTestFixture(
        """foo::create();
          |$bar->create();
          |""".stripMargin
      ){
        val specificCreate : Seq[Method] = cpg.method("foo::create").l
        specificCreate.length shouldBe 1
        val unknownCreate : Seq[Method] = cpg.method("UNKNOWN::create").l
        unknownCreate.length shouldBe 1
      }
      "work with two internal functions" in  new CpgFromCodeTestFixture(
        """header("header");
          |header("header");
          |""".stripMargin
      ) {
        cpg.method("header").l.length shouldBe 1
      }
      "set IS_EXTERNAL for internal functions" in new CpgFromCodeTestFixture("""echo strpos($url, 'pipix');"""){
        cpg.method("strpos").l.head.isExternal shouldBe true
      }
      "create METHOD_RETURN for stub methods" in new CpgFromCodeTestFixture("""echo strpos($url, 'pipix');"""){
        val stub: Method = cpg.method("strpos").l.head
        stub.out(EdgeTypes.AST).collectAll[MethodReturn].size shouldBe 1
        stub.out(EdgeTypes.CFG).collectAll[MethodReturn].size shouldBe 1
      }
    }

  }

}
