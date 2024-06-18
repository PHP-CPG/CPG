package io.joern.bytecode.parser

import java.nio.charset.StandardCharsets
import java.util.Base64

object utils {
  def decodeBase64(str: String): String =
    try {
      new String(Base64.getDecoder.decode(str), StandardCharsets.UTF_8)
    } catch {
      case x: Throwable =>
        throw new RuntimeException(
          s"unable to base64 decode $str: ${x.getMessage}")
    }

  def encodeBase64(str: String): String =
    try {
      Base64.getEncoder.encodeToString(str.getBytes(StandardCharsets.UTF_8))
    } catch {
      case x: Throwable =>
        throw new RuntimeException(
          s"unable to base64 decode $str: ${x.getMessage}")
    }
}
