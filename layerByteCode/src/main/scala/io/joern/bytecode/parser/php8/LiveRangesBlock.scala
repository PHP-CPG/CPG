package io.joern.bytecode.parser.php8

import fastparse.NoWhitespace._
import fastparse._
import io.joern.bytecode.parser.constructs.{LiveRangeLine, LiveRanges}
import io.joern.bytecode.parser.php8.Basics.anyNumber

object LiveRangesBlock {

  def parseLiveRangeType[_: P]: P[String] =
    P("(" ~ ("loop" | "new" | "rope" | "silence" | "tmp/var").! ~ ")")

  def parseLiveRangeLine[_: P]: P[(String, String, String, String)] =
    P(
      " ".rep ~
        anyNumber.rep.! ~ ": " ~
        anyNumber.rep.! ~ " - " ~
        anyNumber.rep.! ~ " " ~/
        parseLiveRangeType)

  def getLiveRangeLine[_: P]: P[LiveRangeLine] =
    P(parseLiveRangeLine.map(x =>
      LiveRangeLine(x._1.toInt, x._2.toInt, x._3.toInt, x._4)))

  def parseLiveRangesBlock[_: P]: P[Seq[LiveRangeLine]] =
    P("LIVE RANGES:\n" ~/ (getLiveRangeLine ~ "\n").rep)

  def getLiveRangesBlock[_: P]: P[LiveRanges] =
    P(parseLiveRangesBlock.map(x => LiveRanges(x)))

}
