package io.joern.bytecode.passes

import io.joern.bytecode.parser.{PHPVersion, PHPVersions}
import io.joern.bytecode.util.unittesting.CpgFromCodeTestFixture
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class QuoteSideCaseTest extends AnyWordSpec with Matchers with PHPVersions {

  for(v <- getPhpVersions) {
    implicit val version: PHPVersion.Value = v

    s"quotes in quotes for PHP $version" should {
      "work for basic case" in new CpgFromCodeTestFixture(
        """
          |echo '") ' . '$var';
          |""".stripMargin
      ) {
        cpg.call("ECHO").astChildren.order(0).head.asInstanceOf[nodes.Literal].code shouldBe "\") $var"
      }
    }

  }
}
