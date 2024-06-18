package io.joern.bytecode.util

object implicits {
  implicit class OneableSeq[T](l: Seq[T]) {

    /**
      * Assert that the Seq only contains one element and return it.
      * @return The only element in the Seq.
      */
    def one: T = {
      assert(l.length == 1)
      l.head
    }
  }
}
