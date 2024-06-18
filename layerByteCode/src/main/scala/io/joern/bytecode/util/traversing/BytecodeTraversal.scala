package io.joern.bytecode.util.traversing

import io.shiftleft.codepropertygraph.generated.nodes.{
  Call,
  CfgNode,
  Identifier,
  Literal
}
import io.shiftleft.semanticcpg.language._

class ByteCodeTraversalHandlingError(message: String) extends Throwable {
  override def getMessage: String = message
}

trait BytecodeTraversal[T] {

  val bytecodeFunctionCalls: Array[String] =
    Array("DO_FCALL", "DO_ICALL", "DO_UCALL", "DO_FCALL_BY_NAME")

  /** traverse on the given node, calling the appropriate function to handle the underlying node type/call
    *
    * @param node the node to traverse on
    * @return the generated return value
    */
  protected def traverse(node: CfgNode): T = {
    node match {
      case ident: Identifier =>
        identifier(ident)
      case lit: Literal =>
        literal(lit)
      case call: Call if bytecodeFunctionCalls.contains(call.name) =>
        functionCall(call)
      case call: Call =>
        traverseBytecodeCall(call)
      case x =>
        throw new RuntimeException(s"unknown how to proceed on ${x.getClass}")
    }
  }

  /** given a cfg node call traverse on the nth child
    *
    * @param cfgNode the cfg node
    * @param nth the child to traverse on
    * @return whatever the implementation shall return
    */
  protected def traverseOnNthChild(cfgNode: CfgNode, nth: Int): T = {
    traverse(cfgNode.astChildren.order(nth).next().asInstanceOf[CfgNode])
  }

  /** Traverse on actual function calls
    *
    * @param call the call node
    * @return whatever the implementation shall return
    */
  protected def functionCall(call: Call): T

  /** Traverse on literals
    *
    * @param literal the literal to process
    * @return whatever the implementation shall return
    */
  protected def literal(literal: Literal): T

  /** Traverse on identifier
    *
    * @param identifier the identifier to process
    * @return whatever the implementation shall return
    */
  protected def identifier(identifier: Identifier): T

  /** deployment function to call the correct processing for the encountered bytecode
    *
    * @param call the bytecode call
    * @return whatever the implementation shall return
    */
  private def traverseBytecodeCall(call: Call): T = {
    call.name match {
      //Array Stuff
      case "INIT_ARRAY"        => INIT_ARRAY(call)
      case "ADD_ARRAY_ELEMENT" => ADD_ARRAY_ELEMENT(call)
      case "IN_ARRAY"          => IN_ARRAY(call)
      case "ADD_ARRAY_UNPACK"  => ADD_ARRAY_UNPACK(call)
      // Assign
      case "ASSIGN_DIM" => ASSIGN_DIM(call)
      case "ASSIGN"     => ASSIGN(call)
      case "ASSIGN_OP"  => ASSIGN_OP(call)
      case "ASSIGN_STATIC_PROP_1" | "ASSIGN_STATIC_PROP_2" =>
        ASSIGN_STATIC_PROP(call)
      case "ASSIGN_STATIC_PROP_OP"                 => ASSIGN_STATIC_PROP_OP(call)
      case "ASSIGN_STATIC_PROP_REF"                => ASSIGN_STATIC_PROP_REF(call)
      case "ASSIGN_OBJ"                            => ASSIGN_OBJ(call)
      case "ASSIGN_REF_2" | "ASSIGN_REF_3"         => ASSIGN_REF(call)
      case "ASSIGN_OBJ_REF_2" | "ASSIGN_OBJ_REF_3" => ASSIGN_OBJ_REF(call)
      case "="                                     => ASSIGN_EQUALSIGN(call)
      //Bind
      case "BIND_STAIC"   => BIND_STATIC(call)
      case "BIND_LEXICAL" => BIND_LEXICAL(call)
      //Call Related
      case "NEW"                     => NEW(call)
      case "INIT_FCALL"              => INIT_FCALL(call)
      case "INIT_METHOD_CALL"        => INIT_METHOD_CALL(call)
      case "INIT_NS_FCALL_BY_NAME"   => INIT_NS_FCALL_BY_NAME(call)
      case "INIT_DYNAMIC_CALL"       => INIT_DYNAMIC_CALL(call)
      case "INIT_FCALL_BY_NAME"      => INIT_FCALL_BY_NAME(call)
      case "INIT_STATIC_METHOD_CALL" => INIT_STATIC_METHOD_CALL(call)
      case "INIT_USER_CALL"          => INIT_USER_CALL(call)
      case "SEND_VAR_EX"             => SEND_VAR_EX(call)
      case "SEND_VAL_EX"             => SEND_VAL_EX(call)
      case "SEND_VAL"                => SEND_VAL(call)
      case "SEND_VAR"                => SEND_VAR(call)
      case "SEND_VAR_NO_REF"         => SEND_VAR_NO_REF(call)
      case "SEND_VAR_NO_REF_EX"      => SEND_VAR_NO_REF_EX(call)
      case "SEND_FUNC_ARG"           => SEND_FUNC_ARG(call)
      case "SEND_USER"               => SEND_USER(call)
      case "SEND_REF"                => SEND_REF(call)
      case "SEND_ARRAY"              => SEND_ARRAY(call)
      case "RECV"                    => RECV(call)
      case "RECV_INIT"               => RECV_INIT(call)
      case "RECV_VARIADIC"           => RECV_VARIADIC(call)
      case "CHECK_FUNC_ARG"          => CHECK_FUNC_ARG(call)
      case "CHECK_UNDEF_ARGS"        => CHECK_UNDEF_ARGS(call)
      case "FUNC_GET_ARG"            => FUNC_GET_ARG(call)
      case "FUNC_GET_ARGS"           => FUNC_GET_ARGS(call)
      case "RETURN_BY_REF"           => RETURN_BY_REF(call)
      case "RETURN"                  => RETURN(call)
      case "VERIFY_RETURN_TYPE"      => VERIFY_RETURN_TYPE(call)
      case "VERIFY_NEVER_TYPE"       => VERIFY_NEVER_TYPE(call)
      case "FAST_RET"                => FAST_RET(call)
      //Class Related
      case "GET_CLASS"          => GET_CLASS(call)
      case "DECLARE_ANON_CLASS" => DECLARE_ANON_CLASS(call)
      case "DECLARE_CLASS"      => DECLARE_CLASS(call)
      //Control Constructs
      case "SWITCH_STRING" => SWITCH_STRING(call)
      case "SWITCH_LONG"   => SWITCH_LONG(call)
      case "MATCH"         => MATCH(call)
      case "CASE_STRICT"   => CASE_STRICT(call)
      case "MATCH_ERROR"   => MATCH_ERROR(call)
      case "EXIT"          => EXIT(call)
      case "CATCH"         => CATCH(call)
      //Fe
      case "FE_FETCH_R"  => FE_FETCH_R(call)
      case "FE_FETCH_RW" => FE_FETCH_RW(call)
      case "FE_RESET_RW" => FE_RESET_RW(call)
      case "FE_RESET_R"  => FE_RESET_R(call)
      //Fetch
      case "FETCH_OBJ_FUNC_ARG"              => FETCH_OBJ_FUNC_ARG(call)
      case "FETCH_STATIC_PROP_R"             => FETCH_STATIC_PROP_R(call)
      case "FETCH_STATIC_PROP_W"             => FETCH_STATIC_PROP_W(call)
      case "FETCH_STATIC_PROP_FUNC_ARG"      => FETCH_STATIC_PROP_FUNC_ARG(call)
      case "FETCH_STATIC_PROP_IS"            => FETCH_STATIC_PROP_IS(call)
      case "FETCH_DIM_R"                     => FETCH_DIM_R(call)
      case "FETCH_LIST_R"                    => FETCH_LIST_R(call)
      case "FETCH_CLASS_CONSTANT"            => FETCH_CLASS_CONSTANT(call)
      case "FETCH_CLASS"                     => FETCH_CLASS(call)
      case "FETCH_CLASS_NAME"                => FETCH_CLASS_NAME(call)
      case "FETCH_DIM_FUNC_ARG"              => FETCH_DIM_FUNC_ARG(call)
      case "FETCH_CONSTANT"                  => FETCH_CONSTANT(call)
      case "FETCH_R"                         => FETCH_R(call)
      case "FETCH_IS"                        => FETCH_IS(call)
      case "FETCH_OBJ_W_2" | "FETCH_OBJ_W_3" => FETCH_OBJ_W(call)
      case "FETCH_STATIC_PROP_UNSET"         => FETCH_STATIC_PROP_UNSET(call)
      case "FETCH_STATIC_PROP_RW"            => FETCH_STATIC_PROP_RW(call)
      case "FETCH_W"                         => FETCH_W(call)
      case "FETCH_DIM_W"                     => FETCH_DIM_W(call)
      case "FETCH_OBJ_RW"                    => FETCH_OBJ_RW(call)
      case "FETCH_OBJ_R"                     => FETCH_OBJ_R(call)
      case "FETCH_RW"                        => FETCH_RW(call)
      case "FETCH_OBJ_IS"                    => FETCH_OBJ_IS(call)
      case "FETCH_DIM_IS"                    => FETCH_DIM_IS(call)
      case "FETCH_DIM_UNSET"                 => FETCH_DIM_UNSET(call)
      case "FETCH_FUNC_ARG"                  => FETCH_FUNC_ARG(call)
      //Generic
      case "NOP"                     => NOP(call)
      case "BEGIN_SILENCE"           => BEGIN_SILENCE(call)
      case "EXT_STMT"                => EXT_STMT(call)
      case "EXT_NOP"                 => EXT_NOP(call)
      case "FETCH_THIS"              => FETCH_THIS(call)
      case "GENERATOR_CREATE"        => GENERATOR_CREATE(call)
      case "GET_CALLED_CLASS"        => GET_CALLED_CLASS(call)
      case "FUNC_NUM_ARGS"           => FUNC_NUM_ARGS(call)
      case "ECHO"                    => ECHO(call)
      case "BW_NOT"                  => BW_NOT(call)
      case "BOOL_NOT"                => BOOL_NOT(call)
      case "QM_ASSIGN"               => QM_ASSIGN(call)
      case "PRE_INC"                 => PRE_INC(call)
      case "POST_INC"                => POST_INC(call)
      case "PRE_DEC"                 => PRE_DEC(call)
      case "POST_DEC"                => POST_DEC(call)
      case "FREE"                    => FREE(call)
      case "PRINT"                   => PRINT(call)
      case "FE_FREE"                 => FE_FREE(call)
      case "END_SILENCE"             => END_SILENCE(call)
      case "BOOL"                    => BOOL(call)
      case "OP_DATA"                 => OP_DATA(call)
      case "THROW"                   => THROW(call)
      case "STRLEN"                  => STRLEN(call)
      case "SEND_UNPACK"             => SEND_UNPACK(call)
      case "COUNT"                   => COUNT(call)
      case "DEFINED"                 => DEFINED(call)
      case "DECLARE_FUNCTION"        => DECLARE_FUNCTION(call)
      case "GET_TYPE"                => GET_TYPE(call)
      case "UNSET_CV"                => UNSET_CV(call)
      case "CLONE"                   => CLONE(call)
      case "MAKE_REF"                => MAKE_REF(call)
      case "SEPARATE"                => SEPARATE(call)
      case "DECLARE_LAMBDA_FUNCTION" => DECLARE_LAMBDA_FUNCTION(call)
      case "GENERATOR_RETURN"        => GENERATOR_RETURN(call)
      case "DISCARD_EXCEPTION"       => DISCARD_EXCEPTION(call)
      case "CONCAT"                  => CONCAT(call)
      case "FAST_CONCAT"             => FAST_CONCAT(call)
      case "ADD"                     => ADD(call)
      case "SUB"                     => SUB(call)
      case "MUL"                     => MUL(call)
      case "DIV"                     => DIV(call)
      case "MOD"                     => MOD(call)
      case "SL"                      => SL(call)
      case "SR"                      => SR(call)
      case "BW_OR"                   => BW_OR(call)
      case "BW_AND"                  => BW_AND(call)
      case "BW_XOR"                  => BW_XOR(call)
      case "BOOL_OR"                 => BOOL_OR(call)
      case "IS_EQUAL"                => IS_EQUAL(call)
      case "IS_NOT_EQUAL"            => IS_NOT_EQUAL(call)
      case "IS_IDENTICAL"            => IS_IDENTICAL(call)
      case "IS_NOT_IDENTICAL"        => IS_NOT_IDENTICAL(call)
      case "IS_SMALLER"              => IS_SMALLER(call)
      case "IS_SMALLER_OR_EQUAL"     => IS_SMALLER_OR_EQUAL(call)
      case "BIND_GLOBAL"             => BIND_GLOBAL(call)
      case "DECLARE_CLASS_DELAYED"   => DECLARE_CLASS_DELAYED(call)
      case "DECLARE_CONST"           => DECLARE_CONST(call)
      case "INCLUDE_OR_EVAL"         => INCLUDE_OR_EVAL(call)
      case "POW"                     => POW(call)
      case "ARRAY_KEY_EXISTS"        => ARRAY_KEY_EXISTS(call)
      case "TYPE_CHECK"              => TYPE_CHECK(call)
      case "FETCH_DIM_RW"            => FETCH_DIM_RW(call)
      case "UNSET_OBJ"               => UNSET_OBJ(call)
      case "FETCH_UNSET"             => FETCH_UNSET(call)
      case "UNSET_DIM"               => UNSET_DIM(call)
      case "CASE"                    => CASE(call)
      case "FETCH_OBJ_UNSET"         => FETCH_OBJ_UNSET(call)
      case "FETCH_GLOBALS"           => FETCH_GLOBALS(call)
      case "UNSET_STATIC_PROP"       => UNSET_STATIC_PROP(call)
      case "POST_INC_OBJ"            => POST_INC_OBJ(call)
      case "PRE_INC_OBJ"             => PRE_INC_OBJ(call)
      case "POST_DEC_OBJ"            => POST_DEC_OBJ(call)
      case "PRE_DEC_OBJ"             => PRE_DEC_OBJ(call)
      case "BOOL_XOR"                => BOOL_XOR(call)
      case "SPACESHIP"               => SPACESHIP(call)
      case "UNSET_VAR"               => UNSET_VAR(call)
      case "ASSIGN_DIM_OP"           => ASSIGN_DIM_OP(call)
      case "ASSIGN_OBJ_OP"           => ASSIGN_OBJ_OP(call)
      //INC DEC STATIC PROP
      case "POST_INC_STATIC_PROP" => POST_INC_STATIC_PROP(call)
      case "PRE_INC_STATIC_PROP"  => PRE_INC_STATIC_PROP(call)
      case "POST_DEC_STATIC_PROP" => POST_DEC_STATIC_PROP(call)
      case "PRE_DEC_STATIC_PROP"  => PRE_DEC_STATIC_PROP(call)
      //ISSET
      case "ISSET_ISEMPTY_VAR"         => ISSET_ISEMPTY_VAR(call)
      case "ISSET_ISEMPTY_DIM_OBJ"     => ISSET_ISEMPTY_DIM_OBJ(call)
      case "ISSET_ISEMPTY_CV"          => ISSET_ISEMPTY_CV(call)
      case "ISSET_ISEMPTY_PROP_OBJ"    => ISSET_ISEMPTY_PROP_OBJ(call)
      case "ISSET_ISEMPTY_STATIC_PROP" => ISSET_ISEMPTY_STATIC_PROP(call)
      case "ISSET_ISEMPTY_THIS"        => ISSET_ISEMPTY_THIS(call)
      //JUMP
      case "JMPNZ"    => JMPNZ(call)
      case "JMPNZ_EX" => JMPNZ_EX(call)
      case "JMPZ"     => JMPZ(call)
      case "JMP"      => JMP(call)
      case "JMPZ_EX"  => JMPZ_EX(call)
      case "JMPZNZ"   => JMPZNZ(call)
      case "JMP_SET"  => JMP_SET(call)
      case "JMP_NULL" => JMP_NULL(call)
      //Lambda related
      case "YIELD"      => YIELD(call)
      case "YIELD_FROM" => YIELD_FROM(call)
      case "TICKS"      => TICKS(call)
      case "FAST_CALL"  => FAST_CALL(call)
      //Rope
      case "ROPE_INIT" => ROPE_INIT(call)
      case "ROPE_ADD"  => ROPE_ADD(call)
      case "ROPE_END"  => ROPE_END(call)
      //Type Related
      case "CAST"             => CAST(call)
      case "INSTANCEOF"       => INSTANCEOF(call)
      case "COALESCE"         => COALESCE(call)
      case "CALLABLE_CONVERT" => CALLABLE_CONVERT(call)
      case _ =>
        call.code match {
          case "DO_FCALL_BY_NAME" => DO_FCALL_BY_NAME(call)
          case "DO_ICALL"         => DO_ICALL(call)
          case "DO_UCALL"         => DO_UCALL(call)
          case "DO_FCALL"         => DO_FCALL(call)
          case x =>
            throw new ByteCodeTraversalHandlingError(
              s"unable to handle bytecode $x")
        }
    }
  }

  protected def INIT_ARRAY(call: Call): T
  protected def ADD_ARRAY_ELEMENT(call: Call): T
  protected def IN_ARRAY(call: Call): T
  protected def DD_ARRAY_UNPACK(call: Call): T
  protected def ADD_ARRAY_UNPACK(call: Call): T
  // Assign
  protected def ASSIGN_DIM(call: Call): T
  protected def ASSIGN(call: Call): T
  protected def ASSIGN_OP(call: Call): T
  protected def ASSIGN_STATIC_PROP(call: Call): T
  protected def ASSIGN_STATIC_PROP_OP(call: Call): T
  protected def ASSIGN_STATIC_PROP_REF(call: Call): T
  protected def ASSIGN_OBJ(call: Call): T
  protected def ASSIGN_REF(call: Call): T
  protected def ASSIGN_OBJ_REF(call: Call): T
  protected def ASSIGN_EQUALSIGN(call: Call): T
  //Bind
  protected def BIND_STATIC(call: Call): T
  protected def BIND_LEXICAL(call: Call): T
  //Call Related
  protected def NEW(call: Call): T
  protected def INIT_FCALL(call: Call): T
  protected def INIT_METHOD_CALL(call: Call): T
  protected def INIT_NS_FCALL_BY_NAME(call: Call): T
  protected def INIT_DYNAMIC_CALL(call: Call): T
  protected def INIT_FCALL_BY_NAME(call: Call): T
  protected def INIT_STATIC_METHOD_CALL(call: Call): T
  protected def INIT_USER_CALL(call: Call): T
  protected def SEND_VAR_EX(call: Call): T
  protected def SEND_VAL_EX(call: Call): T
  protected def SEND_VAL(call: Call): T
  protected def SEND_VAR(call: Call): T
  protected def SEND_VAR_NO_REF(call: Call): T
  protected def SEND_VAR_NO_REF_EX(call: Call): T
  protected def SEND_FUNC_ARG(call: Call): T
  protected def SEND_USER(call: Call): T
  protected def SEND_REF(call: Call): T
  protected def SEND_ARRAY(call: Call): T
  protected def RECV(call: Call): T
  protected def RECV_INIT(call: Call): T
  protected def RECV_VARIADIC(call: Call): T
  protected def CHECK_FUNC_ARG(call: Call): T
  protected def CHECK_UNDEF_ARGS(call: Call): T
  protected def FUNC_GET_ARG(call: Call): T
  protected def FUNC_GET_ARGS(call: Call): T
  protected def RETURN_BY_REF(call: Call): T
  protected def RETURN(call: Call): T
  protected def VERIFY_RETURN_TYPE(call: Call): T
  protected def VERIFY_NEVER_TYPE(call: Call): T
  protected def FAST_RET(call: Call): T
  //Class Related
  protected def GET_CLASS(call: Call): T
  protected def DECLARE_ANON_CLASS(call: Call): T
  protected def DECLARE_CLASS(call: Call): T
  //Control Constructs
  protected def SWITCH_STRING(call: Call): T
  protected def SWITCH_LONG(call: Call): T
  protected def MATCH(call: Call): T
  protected def CASE_STRICT(call: Call): T
  protected def MATCH_ERROR(call: Call): T
  protected def EXIT(call: Call): T
  protected def CATCH(call: Call): T
  //Fe
  protected def FE_FETCH_R(call: Call): T
  protected def FE_FETCH_RW(call: Call): T
  protected def FE_RESET_RW(call: Call): T
  protected def FE_RESET_R(call: Call): T
  //Fetch
  protected def FETCH_OBJ_FUNC_ARG(call: Call): T
  protected def FETCH_STATIC_PROP_R(call: Call): T
  protected def FETCH_STATIC_PROP_W(call: Call): T
  protected def FETCH_STATIC_PROP_FUNC_ARG(call: Call): T
  protected def FETCH_STATIC_PROP_IS(call: Call): T
  protected def FETCH_LIST_R(call: Call): T
  protected def FETCH_CLASS_CONSTANT(call: Call): T
  protected def FETCH_CLASS(call: Call): T
  protected def FETCH_CLASS_NAME(call: Call): T
  protected def FETCH_CONSTANT(call: Call): T
  protected def FETCH_R(call: Call): T
  protected def FETCH_IS(call: Call): T
  protected def FETCH_OBJ_W(call: Call): T
  protected def FETCH_STATIC_PROP_UNSET(call: Call): T
  protected def FETCH_STATIC_PROP_RW(call: Call): T
  protected def FETCH_GLOBALS(call: Call): T
  //Generic
  protected def DO_FCALL_BY_NAME(call: Call): T
  protected def DO_ICALL(call: Call): T
  protected def DO_UCALL(call: Call): T
  protected def DO_FCALL(call: Call): T
  protected def NOP(call: Call): T
  protected def BEGIN_SILENCE(call: Call): T
  protected def EXT_STMT(call: Call): T
  protected def EXT_NOP(call: Call): T
  protected def FETCH_THIS(call: Call): T
  protected def GENERATOR_CREATE(call: Call): T
  protected def GET_CALLED_CLASS(call: Call): T
  protected def FUNC_NUM_ARGS(call: Call): T
  protected def ECHO(call: Call): T
  protected def BW_NOT(call: Call): T
  protected def BOOL_NOT(call: Call): T
  protected def QM_ASSIGN(call: Call): T
  protected def PRE_INC(call: Call): T
  protected def POST_INC(call: Call): T
  protected def PRE_DEC(call: Call): T
  protected def POST_DEC(call: Call): T
  protected def FREE(call: Call): T
  protected def PRINT(call: Call): T
  protected def FE_FREE(call: Call): T
  protected def END_SILENCE(call: Call): T
  protected def BOOL(call: Call): T
  protected def OP_DATA(call: Call): T
  protected def THROW(call: Call): T
  protected def STRLEN(call: Call): T
  protected def SEND_UNPACK(call: Call): T
  protected def COUNT(call: Call): T
  protected def DEFINED(call: Call): T
  protected def DECLARE_FUNCTION(call: Call): T
  protected def GET_TYPE(call: Call): T
  protected def UNSET_CV(call: Call): T
  protected def CLONE(call: Call): T
  protected def MAKE_REF(call: Call): T
  protected def SEPARATE(call: Call): T
  protected def DECLARE_LAMBDA_FUNCTION(call: Call): T
  protected def GENERATOR_RETURN(call: Call): T
  protected def DISCARD_EXCEPTION(call: Call): T
  protected def CONCAT(call: Call): T
  protected def FAST_CONCAT(call: Call): T
  protected def ADD(call: Call): T
  protected def SUB(call: Call): T
  protected def MUL(call: Call): T
  protected def DIV(call: Call): T
  protected def MOD(call: Call): T
  protected def SL(call: Call): T
  protected def SR(call: Call): T
  protected def BW_OR(call: Call): T
  protected def BW_AND(call: Call): T
  protected def BW_XOR(call: Call): T
  protected def BOOL_OR(call: Call): T
  protected def IS_EQUAL(call: Call): T
  protected def IS_NOT_EQUAL(call: Call): T
  protected def IS_IDENTICAL(call: Call): T
  protected def IS_NOT_IDENTICAL(call: Call): T
  protected def IS_SMALLER(call: Call): T
  protected def IS_SMALLER_OR_EQUAL(call: Call): T
  protected def BIND_GLOBAL(call: Call): T
  protected def DECLARE_CLASS_DELAYED(call: Call): T
  protected def DECLARE_CONST(call: Call): T
  protected def INCLUDE_OR_EVAL(call: Call): T
  protected def FETCH_FUNC_ARG(call: Call): T
  protected def FETCH_DIM_FUNC_ARG(call: Call): T
  protected def POW(call: Call): T
  protected def FETCH_DIM_R(call: Call): T
  protected def FETCH_W(call: Call): T
  protected def FETCH_DIM_W(call: Call): T
  protected def ARRAY_KEY_EXISTS(call: Call): T
  protected def FETCH_OBJ_RW(call: Call): T
  protected def FETCH_OBJ_R(call: Call): T
  protected def FETCH_RW(call: Call): T
  protected def FETCH_OBJ_IS(call: Call): T
  protected def FETCH_DIM_IS(call: Call): T
  protected def TYPE_CHECK(call: Call): T
  protected def FETCH_DIM_RW(call: Call): T
  protected def UNSET_OBJ(call: Call): T
  protected def FETCH_UNSET(call: Call): T
  protected def UNSET_DIM(call: Call): T
  protected def FETCH_DIM_UNSET(call: Call): T
  protected def CASE(call: Call): T
  protected def FETCH_OBJ_UNSET(call: Call): T
  protected def UNSET_STATIC_PROP(call: Call): T
  protected def POST_INC_OBJ(call: Call): T
  protected def PRE_INC_OBJ(call: Call): T
  protected def POST_DEC_OBJ(call: Call): T
  protected def PRE_DEC_OBJ(call: Call): T
  protected def BOOL_XOR(call: Call): T
  protected def SPACESHIP(call: Call): T
  protected def UNSET_VAR(call: Call): T
  protected def ASSIGN_DIM_OP(call: Call): T
  protected def ASSIGN_OBJ_OP(call: Call): T
  //INC DEC STATIC PROP
  protected def POST_INC_STATIC_PROP(call: Call): T
  protected def PRE_INC_STATIC_PROP(call: Call): T
  protected def POST_DEC_STATIC_PROP(call: Call): T
  protected def PRE_DEC_STATIC_PROP(call: Call): T
  //ISSET
  protected def ISSET_ISEMPTY_VAR(call: Call): T
  protected def ISSET_ISEMPTY_DIM_OBJ(call: Call): T
  protected def ISSET_ISEMPTY_CV(call: Call): T
  protected def ISSET_ISEMPTY_PROP_OBJ(call: Call): T
  protected def ISSET_ISEMPTY_STATIC_PROP(call: Call): T
  protected def ISSET_ISEMPTY_THIS(call: Call): T
  //JUMP
  protected def JMPNZ(call: Call): T
  protected def JMPNZ_EX(call: Call): T
  protected def JMPZ(call: Call): T
  protected def JMP(call: Call): T
  protected def JMPZ_EX(call: Call): T
  protected def JMPZNZ(call: Call): T
  protected def JMP_SET(call: Call): T
  protected def JMP_NULL(call: Call): T
  //Lambda related
  protected def YIELD(call: Call): T
  protected def YIELD_FROM(call: Call): T
  protected def TICKS(call: Call): T
  protected def FAST_CALL(call: Call): T
  //Rope
  protected def ROPE_INIT(call: Call): T
  protected def ROPE_ADD(call: Call): T
  protected def ROPE_END(call: Call): T
  //Type Related
  protected def CAST(call: Call): T
  protected def INSTANCEOF(call: Call): T
  protected def COALESCE(call: Call): T

  protected def CALLABLE_CONVERT(call: Call): T

}
