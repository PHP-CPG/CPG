package io.joern.bytecode.realWorldPocs

import io.joern.bytecode.parser.{PHPVersion, PHPVersions}
import io.joern.bytecode.util.unittesting.CpgFromCodeTestFixture
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ClosureTest  extends AnyWordSpec with Matchers with PHPVersions {

  for (v <- getPhpVersions) {
    implicit val version: PHPVersion.Value = v

    s"Closures in PHP$version" should {
      "be able to be processed" in new CpgFromCodeTestFixture(
        """
          array_map(fn($n) => $n, array_filter($a['b'], fn($n) => !is_null($n)));
          """.stripMargin
      )
    }

  }

}
