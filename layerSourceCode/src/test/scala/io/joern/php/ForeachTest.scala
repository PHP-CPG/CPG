package io.joern.php

import io.joern.{AbstractCpgTestFixture, TraversalUtils}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ForeachTest extends AnyWordSpec with Matchers with TraversalUtils {

  val fixture: AbstractCpgTestFixture = CpgFromCodeTestFixture(
    """
      |foreach($array as $key => $value) {
      |   echo $key;
      |}
      |foreach($array as $value) {
      |   echo $x;
      |}
      |""".stripMargin)

}
