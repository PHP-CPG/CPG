package io.joern.bytecode.parser.constructs

sealed trait Opcode {
  val code: String
}

case class NoValueOperation(code: String) extends Opcode

case class SingleValueOperation(code: String, value: Value) extends Opcode

case class DualValueOperation(code: String, lhs: Value, rhs: Value)
    extends Opcode

case class TripleValueOperation(code: String,
                                first: Value,
                                second: Value,
                                third: Value)
    extends Opcode

case class QuadrupleValueOperation(code: String,
                                   first: Value,
                                   second: Value,
                                   third: Value,
                                   fourth: Value)
    extends Opcode

case class QuintupleValueOperation(code: String,
                                   first: Value,
                                   second: Value,
                                   third: Value,
                                   fourth: Value,
                                   fifth: Value)
    extends Opcode


case class MatchOpcode(code: String,
                       matchee: Variable,
                       values: Seq[KeyValuePair],
                       default: String)
    extends Opcode


case class INIT_FCALL(paramCount: Int, var2: Int, var function: StringLiteral)
    extends Opcode {
  val code = "INIT_FCALL"
  function = StringLiteral(function.value.toLowerCase())
}

case class INIT_DYNAMIC_CALL(paramCount: Int, variable: Variable)
    extends Opcode {
  val code = "INIT_DYNAMIC_CALL"
}

case class INIT_FCALL_BY_NAME(paramCount: Int, var function: String)
    extends Opcode {
  val code = "INIT_FCALL_BY_NAME"
  function = function.toLowerCase
}

case class INIT_METHOD_CALL(paramCount: Int,
                            objectVar: Variable,
                            var method: Value)
    extends Opcode {
  val code = "INIT_METHOD_CALL"
  method = method match {
    case StringLiteral(x) => StringLiteral(x.toLowerCase())
    case x: Variable      => x
    case _ =>
      throw new RuntimeException(s"unexpected value as method $method")
  }
}

case class INIT_NS_FCALL_BY_NAME(paramCount: Int, var function: String)
    extends Opcode {
  val code = "INIT_FS_FCALL_BY_NAME"
  function = function.toLowerCase
}

case class INIT_STATIC_METHOD_CALL(paramCount: Int,
                                   firstKeyWord: Option[Value],
                                   secondKeyword: Option[Value],
                                   var baseClass: Option[Value],
                                   var method: Value)
    extends Opcode {
  val code = "INIT_STATIC_METHOD_CALL"
  baseClass = baseClass match {
    case Some(name: StringLiteral) =>
      Some(StringLiteral(name.value.toLowerCase))
    case x => x
  }
  method = method match {
    case StringLiteral(value) => StringLiteral(value.toLowerCase())
    case x                    => x
  }
}

case class INIT_USER_CALL(param_count: Int,
                          func_type: StringLiteral,
                          act_on: Value)
    extends Opcode {
  val code = "INIT_USER_CALL"
}

case class SWITCH(code: String, value: Value, switches: Seq[(String, Int)])
    extends Opcode
