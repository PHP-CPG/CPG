package io.joern.php

import io.joern.{AbstractCpgTestFixture, TraversalUtils}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ArrayTest extends AnyWordSpec with Matchers with TraversalUtils {

  val fixture: AbstractCpgTestFixture = CpgFromCodeTestFixture(
    """$test2 = array(1,2,3);
      |$test = array();
      |$test[] = 13;
      |$test['test'] = 42;
      |$test[2] = 33;
      |""".stripMargin)

}
