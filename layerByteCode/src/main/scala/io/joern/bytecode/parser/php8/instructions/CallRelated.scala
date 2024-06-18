package io.joern.bytecode.parser.php8.instructions

import fastparse.NoWhitespace._
import fastparse._
import io.joern.bytecode.parser.constructs._
import io.joern.bytecode.parser.php8.Basics.anyNumber
import io.joern.bytecode.parser.php8.Literals.{
  getAnyLiteral,
  getByteCodeKeyword,
  getStringLiteral
}
import io.joern.bytecode.parser.php8.Variables.getAnyVariable
import io.joern.bytecode.parser.php8.instructions.Utility.getAnyValue
import io.joern.reporting.ReportableError

object CallRelated {

  def parseNew[_: P]: P[(String, Seq[Value])] =
    P("NEW " ~ anyNumber.rep.! ~ (" " ~ getAnyValue).rep)
  def getNew[_: P]: P[Opcode] =
    P(parseNew.map {
      case (args, first :: second :: Nil) =>
        TripleValueOperation("NEW", IntegerLiteral(args.toLong), first, second)
      case (args, first :: Nil) =>
        DualValueOperation("NEW", IntegerLiteral(args.toLong), first)
      case x =>
        throw ReportableError("",
                              -1,
                              "",
                              "",
                              s"when parsing New unexpected result tuple $x")
    })

  def parseInitFcall[_: P]: P[(String, String, StringLiteral)] =
    P("INIT_FCALL" ~/ " " ~ anyNumber.rep.! ~ " " ~ anyNumber.rep.! ~ " " ~ getStringLiteral)
  def getInitFcall[_: P]: P[INIT_FCALL] =
    P(parseInitFcall.map(x => INIT_FCALL(x._1.toInt, x._2.toInt, x._3)))

  def parseInitMethodCall[_: P]: P[(String, Object, Value)] =
    P("INIT_METHOD_CALL " ~/ anyNumber.rep.! ~ " " ~ ("THIS".! | getAnyVariable) ~ " " ~ getAnyValue)
  def getInitMethodCall[_: P]: P[INIT_METHOD_CALL] =
    P(
      parseInitMethodCall.map(x =>
        INIT_METHOD_CALL(
          x._1.toInt,
          x._2 match {
            case x: Variable => x
            case x: String   => Variable(x, tmp = false, reference = true)
            case _ =>
              throw new RuntimeException("unknown object reference in call")
          },
          x._3
      )))

  def parseInitNsFcallByName[_: P]: P[(String, StringLiteral)] =
    P("INIT_NS_FCALL_BY_NAME " ~/ anyNumber.rep.! ~ " " ~ getStringLiteral)
  def getInitNsFcallByName[_: P]: P[INIT_NS_FCALL_BY_NAME] =
    P(parseInitNsFcallByName.map(x =>
      INIT_NS_FCALL_BY_NAME(x._1.toInt, x._2.value)))

  def parseInitDynamicCall[_: P]: P[(String, Variable)] =
    P("INIT_DYNAMIC_CALL " ~/ anyNumber.rep.! ~ " " ~ getAnyVariable)
  def getInitDynamicCall[_: P]: P[INIT_DYNAMIC_CALL] =
    P(parseInitDynamicCall.map(x => INIT_DYNAMIC_CALL(x._1.toInt, x._2)))

  def parseInitFcallByName[_: P]: P[(String, StringLiteral)] =
    P("INIT_FCALL_BY_NAME " ~/ anyNumber.rep.! ~ " " ~ getStringLiteral)
  def getInitFcallByName[_: P]: P[INIT_FCALL_BY_NAME] =
    P(parseInitFcallByName.map(x => INIT_FCALL_BY_NAME(x._1.toInt, x._2.value)))

  def parseInitStaticMethodCallA[_: P]: P[(String, Seq[Value])] =
    P("INIT_STATIC_METHOD_CALL " ~/ anyNumber.rep.! ~ (" " ~ getAnyValue).rep)
  def getInitStaticMethodCallA[_: P]: P[INIT_STATIC_METHOD_CALL] =
    P(parseInitStaticMethodCallA.map {
      case (args, first :: second :: Nil) =>
        INIT_STATIC_METHOD_CALL(args.toInt, None, None, Some(first), second)
      case (args, first :: second :: third :: Nil) =>
        INIT_STATIC_METHOD_CALL(args.toInt,
                                Some(first),
                                None,
                                Some(second),
                                third)
      case (args, first :: second :: third :: fourth :: Nil) =>
        INIT_STATIC_METHOD_CALL(args.toInt,
                                Some(first),
                                Some(second),
                                Some(third),
                                fourth)
      case x =>
        throw ReportableError(
          "",
          -1,
          "",
          "",
          s"when parsing InitStaticMethodCall unexpected result tuple $x")
    })

  def parseInitStaticMethodCallB[_: P]
    : P[(String, ByteCodeKeyword, ByteCodeKeyword)] =
    P("INIT_STATIC_METHOD_CALL " ~
      anyNumber.rep.! ~ " " ~ getByteCodeKeyword ~ " " ~ getByteCodeKeyword ~ " " ~ "CONSTRUCTOR")

  def getInitStaticMethodCallB[_: P]: P[QuadrupleValueOperation] =
    P(
      parseInitStaticMethodCallB.map(
        x =>
          QuadrupleValueOperation("INIT_STATIC_METHOD_CALL",
                                  IntegerLiteral(x._1.toLong),
                                  x._2,
                                  x._3,
                                  StringLiteral("CONSTRUCTOR"))
      ))

  def getInitStaticMethodCall[_: P]: P[Opcode] =
    P(getInitStaticMethodCallB | getInitStaticMethodCallA)

  def parseInitUserCall[_: P]: P[(String, StringLiteral, Value)] =
    P("INIT_USER_CALL " ~/ anyNumber.rep.! ~ " " ~ getStringLiteral ~ " " ~ getAnyValue)

  def getInitUserCall[_: P]: P[Opcode] =
    P(parseInitUserCall.map(x => INIT_USER_CALL(x._1.toInt, x._2, x._3)))

  def parseInitCallCommands[_: P]: P[Opcode] = P(
    &("INIT_FCALL ") ~ getInitFcall |
      &("INIT_METHOD_CALL ") ~ getInitMethodCall |
      &("INIT_NS_FCALL_BY_NAME ") ~ getInitNsFcallByName |
      &("INIT_DYNAMIC_CALL ") ~ getInitDynamicCall |
      &("INIT_FCALL_BY_NAME ") ~ getInitFcallByName |
      &("INIT_STATIC_METHOD_CALL ") ~ getInitStaticMethodCall |
      &("INIT_USER_CALL ") ~ getInitUserCall
  )

  def getPayloadReference[_: P]: P[Value] =
    P(anyNumber.rep(1).!.map(x => IntegerLiteral(x.toLong)) | getStringLiteral)

  def parseSendVarEx[_: P]: P[(Value, Value)] =
    P(getAnyValue ~ " " ~ getPayloadReference)
  def getSendVarEx[_: P]: P[DualValueOperation] =
    P(parseSendVarEx.map(x => DualValueOperation("SEND_VAR_EX", x._1, x._2)))

  def parseSendValEx[_: P]: P[(Value, Value)] =
    P(getAnyValue ~ " " ~ getPayloadReference)
  def getSendValEx[_: P]: P[DualValueOperation] =
    P(parseSendVarEx.map { x =>
      DualValueOperation("SEND_VAL_EX", x._1, x._2)
    })

  def parseSendVal[_: P]: P[(Value, Value)] =
    P(getAnyValue ~ " " ~ getPayloadReference)
  def getSendVal[_: P]: P[DualValueOperation] =
    P(parseSendVal.map { x =>
      //assert(x._2 != "", s"after parsing ${x._1} we encounter an empty integer string")
      DualValueOperation("SEND_VAL", x._1, x._2)
    })

  def parseSendVar[_: P]: P[(Value, Value)] =
    P(getAnyValue ~ " " ~ getPayloadReference)
  def getSendVar[_: P]: P[DualValueOperation] = {
    P(parseSendVar.map(x => DualValueOperation("SEND_VAR", x._1, x._2)))
  }

  def parseSendVarNoRefEx[_: P]: P[(Variable, Value)] =
    P(getAnyVariable ~ " " ~ getPayloadReference)
  def getSendVarNoRefEx[_: P]: P[DualValueOperation] =
    P(parseSendVarNoRefEx.map(x =>
      DualValueOperation("SEND_VAR_NO_REF_EX", x._1, x._2)))

  def parseSendVarNoRef[_: P]: P[(Variable, Value)] =
    P(getAnyVariable ~ " " ~ getPayloadReference)
  def getSendVarNoRef[_: P]: P[DualValueOperation] =
    P(parseSendVarNoRef.map(x =>
      DualValueOperation("SEND_VAR_NO_REF", x._1, x._2)))

  def parseSendFuncVar[_: P]: P[(Variable, Value)] =
    P(getAnyVariable ~ " " ~ getPayloadReference)
  def getSendFuncVar[_: P]: P[DualValueOperation] =
    P(parseSendFuncVar.map(x =>
      DualValueOperation("SEND_FUNC_ARG", x._1, x._2)))

  def parseSendUser[_: P]: P[(Value, Value)] =
    P(getAnyValue ~ " " ~ getPayloadReference)
  def getSendUser[_: P]: P[DualValueOperation] =
    P(parseSendUser.map(x => DualValueOperation("SEND_USER", x._1, x._2)))

  def parseSendRef[_: P]: P[(Value, Value)] = {
    P("SEND_REF " ~ getAnyValue ~ " " ~ getPayloadReference)
  }
  def getSendRef[_: P]: P[DualValueOperation] = P(
    parseSendRef.map(x => DualValueOperation("SEND_REF", x._1, x._2))
  )

  // 2021-12-14: not sure if this should also use getPayloadReference /Malte
  def parseSendArray[_: P]: P[(String, Seq[Value])] =
    P("SEND_ARRAY " ~/ anyNumber.rep.! ~ (" " ~ getAnyValue).rep)

  def getSendArray[_: P]: P[Opcode] =
    P(parseSendArray.map { x =>
      x._2 match {
        case first :: Nil =>
          DualValueOperation("SEND_ARRAY", IntegerLiteral(x._1.toLong), first)
        case first :: second :: Nil =>
          TripleValueOperation("SEND_ARRAY",
                               IntegerLiteral(x._1.toLong),
                               first,
                               second)
      }
    })

  def getSendCommand[_: P]: P[Opcode] =
    P(
      "SEND_VAR_NO_REF_EX " ~/ getSendVarNoRefEx |
        "SEND_VAR_NO_REF " ~/ getSendVarNoRef |
        "SEND_VAL_EX " ~/ getSendValEx |
        "SEND_VAR_EX " ~/ getSendVarEx |
        "SEND_VAL " ~/ getSendVal |
        "SEND_VAR " ~/ getSendVar |
        "SEND_USER " ~/ getSendUser |
        "SEND_FUNC_ARG " ~/ getSendFuncVar |
        &("SEND_REF ") ~/ getSendRef |
        &("SEND_ARRAY ") ~/ getSendArray)

  def parseRecv[_: P]: P[String] = P("RECV " ~/ anyNumber.rep.!)
  def getRecv[_: P]: P[SingleValueOperation] =
    P(parseRecv.map(x =>
      SingleValueOperation("RECV", IntegerLiteral(x.toLong))))

  def parseRecvInit[_: P]: P[(String, Value)] =
    P("RECV_INIT " ~/ anyNumber.rep.! ~ " " ~/ getAnyLiteral)
  def getRecvInit[_: P]: P[DualValueOperation] =
    P(parseRecvInit.map(x =>
      DualValueOperation("RECV_INIT", IntegerLiteral(x._1.toLong), x._2)))

  def parseRecvVariadic[_: P]: P[String] =
    P("RECV_VARIADIC" ~ " " ~ anyNumber.rep.!)
  def getRecvVariadic[_: P]: P[Opcode] =
    P(parseRecvVariadic.map(x =>
      SingleValueOperation("RECV_VARIADIC", IntegerLiteral(x.toLong))))

  def parseCheckFuncArg[_: P]: P[Value] =
    P(
      "CHECK_FUNC_ARG " ~/ (anyNumber
        .rep(1)
        .!
        .map(x => IntegerLiteral(x.toLong)) | getStringLiteral))
  def getCheckFuncArg[_: P]: P[SingleValueOperation] =
    P(parseCheckFuncArg.map(x => SingleValueOperation("CHECK_FUNC_ARG", x)))

  def parseFuncGetArgs[_: P]: P[Seq[Value]] =
    P(&("FUNC_GET_ARGS") ~ "FUNC_GET_ARGS" ~ (" " ~ getAnyValue).rep)
  def getFuncGetArgs[_: P]: P[Opcode] =
    P(parseFuncGetArgs.map {
      case Nil           => NoValueOperation("FUNC_GET_ARGS")
      case single :: Nil => SingleValueOperation("FUNC_GET_ARGS", single)
    })

  def parseReturnByRef[_: P]: P[Seq[Value]] =
    P("RETURN_BY_REF" ~ (" " ~ getAnyValue).rep)
  def getReturnByRef[_: P]: P[Opcode] =
    P(parseReturnByRef.map {
      case first :: Nil => SingleValueOperation("RETURN_BY_REF", first)
      case first :: second :: Nil =>
        DualValueOperation("RETURN_BY_REF", first, second)
    })

  def parseReturn[_: P]: P[Value] =
    P("RETURN " ~ getAnyValue)
  def getReturn[_: P]: P[Opcode] =
    P(parseReturn.map(SingleValueOperation("RETURN", _)))

  def getReturnCommand[_: P]: P[Opcode] =
    P(
      &("RETURN_BY_REF ") ~ getReturnByRef |
        &("RETURN ") ~ getReturn)

  def parseVerifyReturnType[_: P]: P[Seq[Value]] =
    P("VERIFY_RETURN_TYPE" ~ (" " ~ getAnyValue).rep ~ &("\n" | End))
  def getVerifyReturnType[_: P]: P[Opcode] =
    P(parseVerifyReturnType.map {
      case Nil           => NoValueOperation("VERIFY_RETURN_TYPE")
      case single :: Nil => SingleValueOperation("VERIFY_RETURN_TYPE", single)
      case list =>
        throw new UnexpectedArgumentCount("VERIFY_RETURN_TYPE",
                                          Seq(1, 2),
                                          list.length)
    })

  def parseFastRet[_: P]: P[Seq[Value]] =
    P(&("FAST_RET") ~ "FAST_RET" ~ (" " ~ getAnyValue).rep)
  def getFastRet[_: P]: P[Opcode] =
    P(parseFastRet.map {
      case first :: Nil => SingleValueOperation("FAST_RET", first)
      case first :: second :: Nil =>
        DualValueOperation("FAST_RET", first, second)
      case list =>
        throw new UnexpectedArgumentCount("FAST_RET", Seq(1, 2), list.length)
    })

}
