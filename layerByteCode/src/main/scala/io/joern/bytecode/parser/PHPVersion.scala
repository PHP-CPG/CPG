package io.joern.bytecode.parser

import io.joern.bytecode.parser.PHPVersion.{V7, V8}

object PHPVersion extends Enumeration {
  type PHPVersion = Value
  val V7, V8 = Value
}

trait PHPVersions {

  def getPhpVersions = Set(V7, V8)

}
