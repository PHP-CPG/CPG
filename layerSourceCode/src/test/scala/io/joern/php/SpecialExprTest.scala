package io.joern.php

import io.joern.{AbstractCpgTestFixture, TraversalUtils}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SpecialExprTest extends AnyWordSpec with Matchers with TraversalUtils {

  val fixture: AbstractCpgTestFixture = CpgFromCodeTestFixture(
    """ `ls -la`;
      |empty($test);
      |function def(int $test) {}
      |"test ${var}";
      |['id' => 1,
      | 'title' => 2 ];
      | function($test) use ($test) {
      |     echo $test;
      | };
      |$a ? $b : $c;
      |""".stripMargin)

}
