package io.joern.php

import io.joern.{AbstractCpgTestFixture, TraversalUtils}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class UnaryOpTest extends AnyWordSpec with Matchers with TraversalUtils {

  val fixture: AbstractCpgTestFixture = CpgFromCodeTestFixture(
    """!$test;
      |-$test;
      |+$test;
      |~$test;
      |++$test;
      |--$test;
      |$test++;
      |$test--;
      |@test();
      |""".stripMargin
  )

  //@test()
  //`ls -la`
}
