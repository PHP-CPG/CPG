package io.joern.php

import io.joern.TraversalUtils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TryCatchTest extends AnyWordSpec with Matchers with TraversalUtils {

  override val fixture: CpgFromCodeTestFixture = CpgFromCodeTestFixture("""
      |try {
      |   echo "20";
      |} catch (Exception $e) {
      |   echo $e;
      |} finally {
      |  echo "finally";
      |}
      |""".stripMargin)

}
