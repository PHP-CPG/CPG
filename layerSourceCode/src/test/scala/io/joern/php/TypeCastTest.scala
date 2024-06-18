package io.joern.php

import io.joern.{AbstractCpgTestFixture, TraversalUtils}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TypeCastTest extends AnyWordSpec with Matchers with TraversalUtils {

  val fixture: AbstractCpgTestFixture = CpgFromCodeTestFixture("""
      |(string)42;
      |(int)3.0;
      |""".stripMargin)

}
