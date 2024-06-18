package io.joern.php

import io.joern.TraversalUtils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class PropertyFetchTest extends AnyWordSpec with Matchers with TraversalUtils {

  override val fixture: CpgFromCodeTestFixture = CpgFromCodeTestFixture("""
        |static::$value;
        |static::$value->function();
        |$value::property;
        |""".stripMargin)

}
