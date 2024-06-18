package io.joern.bytecode.parser.php8.instructions

class UnexpectedArgumentCount(opcode: String,
                              expected: Seq[Int],
                              encountered: Int)
    extends Throwable {
  override def getMessage: String =
    s"$opcode was encountered with $encountered args but only {${expected.mkString(",")}} are supported"
}
