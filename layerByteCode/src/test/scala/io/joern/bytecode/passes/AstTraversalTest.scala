package io.joern.bytecode.passes

import io.joern.bytecode.parser
import io.joern.bytecode.parser.PHPVersions
import io.joern.bytecode.passes.utility.AstTraversal
import io.joern.bytecode.util.unittesting.CpgFromCodeTestFixture
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class AstTraversalTest  extends AnyWordSpec with Matchers with PHPVersions {

  for(v <- getPhpVersions) {
    implicit val version: parser.PHPVersion.Value = v

    s"getParentMethod for PHP $version" should {
      "work with closures" in new CpgFromCodeTestFixture(
        """function test() {
          |   $stuff = 42;
          |   function ($param) use ($stuff) {
          |       echo "dostuff";
          |   };
          |}
          |""".stripMargin
      ) {
        //we do not process any closures anymore and this unit test only tests wether there is a crash
        //val echo: Call = cpg.call.name("ECHO").next()
        //AstTraversal.getParentMethod(echo).name shouldBe "closure"
      }
    }
  }

}
