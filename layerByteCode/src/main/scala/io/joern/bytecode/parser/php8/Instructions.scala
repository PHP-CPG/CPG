package io.joern.bytecode.parser.php8

import fastparse.NoWhitespace._
import fastparse._
import io.joern.bytecode.parser.constructs
import io.joern.bytecode.parser.constructs._
import io.joern.bytecode.parser.php8.Basics.anyNumber
import io.joern.bytecode.parser.php8.Variables.getAnyVariable
import io.joern.bytecode.parser.php8.instructions.Array._
import io.joern.bytecode.parser.php8.instructions.Assign._
import io.joern.bytecode.parser.php8.instructions.Bind._
import io.joern.bytecode.parser.php8.instructions.CallRelated._
import io.joern.bytecode.parser.php8.instructions.ClassRelated._
import io.joern.bytecode.parser.php8.instructions.ControlConstructs._
import io.joern.bytecode.parser.php8.instructions.Fe._
import io.joern.bytecode.parser.php8.instructions.Fetch._
import io.joern.bytecode.parser.php8.instructions.Generic._
import io.joern.bytecode.parser.php8.instructions.IncDecStaticProp._
import io.joern.bytecode.parser.php8.instructions.Isset._
import io.joern.bytecode.parser.php8.instructions.Jump._
import io.joern.bytecode.parser.php8.instructions.LambdaRelated._
import io.joern.bytecode.parser.php8.instructions.MatchRelated._
import io.joern.bytecode.parser.php8.instructions.Rope._
import io.joern.bytecode.parser.php8.instructions.TypeRelated._
import io.joern.bytecode.parser.php8.instructions.Utility.getAnyValue

object Instructions {

  // here we may extend the available options each time we create a new one
  def parseOperation[_: P]: P[Opcode] =
    P(parseInitCallCommands | getNew | getInitMethodCall | getSwitchStatement | //order is important as INIT_FCALL is substring of INIT_FCALL_BY_NAME
      parseAssignCommand | getCheckFuncArg |
      getSendCommand | getRecv | getRecvInit | getRecvVariadic | getTicks | getFuncGetArgs | getIssetIsEmptyStaticProp |
      getFeReset | parseFetchCommand | getAssignDim | getReturnCommand | getBindLexical | getInstanceOf | getGetClass |
      getRopeInit | getRopeAdd | getRopeEnd | getCast | getFeResetRw | getFeFetch | getBindStatic | getFastCall |
      getJmpCommand | getInitArray | getAddArrayElement | getInArray | getExit | getCatch | getCoalesce | getYield | getFastRet |
      getIssetCommand | getQuadrupleValueCommand | getVerifyReturnType | getDeclareClass | getIncDecStaticProp | getAddArrayUnpack |
      getDeclareFunction | getDeclareLambdaFunction | getMatch | getTypeCheck |
      getTripleValueCommand | getDualValueCommand | getSingleValueCommand | getNoValueCommand | getDeclareAnonClass) //jumps

  def getOperation[_: P]: P[Operation] =
    P(parseOperation.map(x => constructs.Operation(x)))

  def parseAssignment[_: P]: P[(Variable, Operation)] =
    P(getAnyVariable ~ " = " ~ getOperation)

  def getAssignment[_: P]: P[Assignment] =
    P(parseAssignment.map(x => constructs.Assignment(x._1, x._2.op)))

  def getInstruction[_: P]: P[Instruction] = P(getAssignment | getOperation)

  def getDeclareFunction[_: P]: P[DualValueOperation] =
    P("DECLARE_FUNCTION".! ~/ " " ~ getAnyValue ~ " " ~ anyNumber.rep.!)
      .map(x => DualValueOperation(x._1, x._2, IntegerLiteral(x._3.toLong)))

  def parseDeclareLambdaFunction[_: P]: P[Value] = {
    P(
      "DECLARE_LAMBDA_FUNCTION" ~/ " " ~ (getAnyValue | anyNumber
        .rep(1)
        .!
        .map(x => IntegerLiteral(x.toLong))))
  }
  def getDeclareLambdaFunction[_: P]: P[SingleValueOperation] = {
    P(parseDeclareLambdaFunction).map(
      SingleValueOperation("DECLARE_LAMBDA_FUNCTION", _))
  }
}
