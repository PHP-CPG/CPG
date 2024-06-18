package io.joern.bytecode.parser.php8.instructions

import fastparse.NoWhitespace._
import fastparse.{P, _}
import io.joern.bytecode.parser.constructs.{
  DefaultKeyValuePair,
  KeyValuePair,
  MatchOpcode,
  Variable
}
import io.joern.bytecode.parser.php8.Basics.anyNumber
import io.joern.bytecode.parser.php8.Variables.getAnyVariable
import io.joern.bytecode.parser.php8.instructions.Utility.{
  parseStringInQuotes,
  parseTarget
}
import io.joern.bytecode.parser.utils.decodeBase64

object MatchRelated {

  def getMatchDefaultTuple[_: P]: P[DefaultKeyValuePair] =
    P("default: " ~ parseTarget).map(x => DefaultKeyValuePair(x))

  // example: >>"apple": 0005<<
  // >>1: 0005<<
  def parseMatchValueTuple[_: P]: P[(Either[Int, String], String)] = {
    P(
      (parseStringInQuotes.map(x => Right(decodeBase64(x))) | anyNumber
        .rep(1)
        .!
        .map(_.toInt)
        .map(Left(_))) ~ ": " ~ parseTarget)
  }

  def getMatchValueTuple[_: P]: P[KeyValuePair] =
    P(parseMatchValueTuple).map(x => KeyValuePair(x._1, x._2))

  // example: MATCH CV0($food) "apple": 0005, "bar": 0007, "cake": 0009, "apple2": 0011, "bar2": 0013, "cake2": 0015, default: 0004

  def parseMatch[_: P]
    : P[(String, Variable, Seq[KeyValuePair], DefaultKeyValuePair)] = {
    // 2021-12-14: it was a conscious decision to not cut after the MATCH, as we also have MATCH_ERROR
    P("MATCH".! ~ " " ~ getAnyVariable ~ " " ~ (getMatchValueTuple ~ ", ").rep ~ getMatchDefaultTuple)
  }

  def getMatch[_: P]: P[MatchOpcode] =
    P(parseMatch).map(x => MatchOpcode(x._1, x._2, x._3, x._4.value))

}
