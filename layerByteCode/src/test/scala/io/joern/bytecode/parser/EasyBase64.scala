package io.joern.bytecode.parser

import java.nio.charset.StandardCharsets
import java.util.Base64

object EasyBase64 {

  def encode(str : String) : String = {
    Base64.getEncoder.encodeToString(str.getBytes(StandardCharsets.UTF_8))
  }

}
