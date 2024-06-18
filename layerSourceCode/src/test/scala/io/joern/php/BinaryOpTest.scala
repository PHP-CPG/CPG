package io.joern.php

import io.joern.{AbstractCpgTestFixture, TraversalUtils}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BinaryOpTest extends AnyWordSpec with Matchers with TraversalUtils {

  val fixture: AbstractCpgTestFixture = CpgFromCodeTestFixture(
    """$a + $b;
      |$a - $b;
      |$a * $b;
      |$a / $b;
      |$a % $b;
      |$a ** $b;
      |$a & $b;
      |$a | $b;
      |$a << $b;
      |$a >> $b;
      |$a . $b;
      |$a ?? $b;
      |$a == $b;
      |$a === $b;
      |$a != $b;
      |$a <> $b;
      |$a !== $b;
      |$a < $b;
      |$a > $b;
      |$a <= $b;
      |$a <=> $b;
      |$a and $b;
      |$a && $b;
      |$a or $b;
      |$a || $b;
      |$a . $b;
      |$a instanceof $b;
      |$a xor $b;
      |""".stripMargin
  )

}
