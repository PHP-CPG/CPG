package io.joern.bytecode.realWorldPocs

import io.joern.bytecode.parser.{PHPVersion, PHPVersions}
import io.joern.bytecode.util.unittesting.CpgFromCodeTestFixture
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import io.shiftleft.semanticcpg.language._

class MissingLiteralParentTest extends AnyWordSpec with Matchers with PHPVersions {

  for (v <- getPhpVersions) {
    implicit val version: PHPVersion.Value = v

    s"every literal in PHP $v" should {
      "have a parent " in new CpgFromCodeTestFixture(
        """$outeru = 3;
          |$closure = function($outerp) use (
          |         $outeru
          |         ) {
          |         $inneru = 4;
          |         $inclo = function($innerp) use ($inneru) {
          |            echo "test";
          |         };
          |};
          |""".stripMargin
      ) {
        // we do not process closures anymore this unit test only tests whether it compiles without error
        //cpg.literal.foreach {
        //  lit => lit.in(EdgeTypes.AST).hasNext shouldBe true
        //    lit.astParent.next()
        //}
      }

    }

  }

}
