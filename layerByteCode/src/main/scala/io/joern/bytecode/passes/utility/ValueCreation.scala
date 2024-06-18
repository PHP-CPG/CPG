package io.joern.bytecode.passes.utility

import io.joern.bytecode.parser.constructs._
import io.shiftleft.codepropertygraph.generated.nodes
import spray.json._
import MyJsonProtocol._
import io.joern.bytecode.parser.utils.encodeBase64
object ValueCreation {

  def createValueNode(value: Value, order: Integer): (nodes.NewNode, String) = {
    assert(order >= 0)
    value match {
      case Variable(name, ref, tmp) =>
        (nodes
           .NewIdentifier()
           .name(name)
           .code(if (ref || tmp) s"$name" else s"CV($$$name)")
           .order(order),
         if (ref || tmp) {
           name
         } else {
           if (ref || tmp) s"$name" else "CV($" + name + ")"
         })
      case ByteCodeConstructor() =>
        (nodes
           .NewLiteral()
           .typeFullName("ByteCodeConstructor")
           .code("CONSTRUCTOR")
           .order(order),
         s"CONSTRUCTOR")
      case AssignOpLiteral(value) =>
        (nodes
           .NewLiteral()
           .typeFullName("AssignOp")
           .code(value)
           .order(order),
         s"($value)")
      case StringLiteral(value) =>
        (nodes.NewLiteral().typeFullName("String").code(value).order(order),
         "string(\"" + value + "\")")
      case IntegerLiteral(value) =>
        (nodes
           .NewLiteral()
           .typeFullName("Integer")
           .code(s"$value")
           .order(order),
         s"int($value)")
      case Null() =>
        (nodes.NewLiteral().typeFullName("NULL").code(" null").order(order),
         "NULL")
      case FloatLiteral(value) =>
        (nodes
           .NewLiteral()
           .typeFullName("Float")
           .code(s"$value")
           .order(order),
         s"float($value)")
      case BooleanLiteral(value) =>
        (nodes
           .NewLiteral()
           .typeFullName("Boolean")
           .code(s"$value")
           .order(order),
         s"bool($value)")
      case TryCatchLiteral(value) =>
        (nodes
           .NewLiteral()
           .typeFullName("try-catch")
           .code(s"$value")
           .order(order),
         s"try-catch($value)")
      case av: ArrayValue =>
        av match {
          case ArrayValue(None) =>
            (nodes.NewLiteral().typeFullName("Array").order(order), "array(..)")
          case ArrayValue(Some(content)) =>
            content match {
              case ::(_, _) =>
                val json = encodeBase64(av.toJson.toString())
                val code = s"array($json)"
                (nodes
                   .NewLiteral()
                   .typeFullName("Array")
                   .order(order)
                   .code(code),
                 code)
              case Nil =>
                val code = s"array()"
                (nodes
                   .NewLiteral()
                   .typeFullName("Array")
                   .order(order)
                   .code(code),
                 code)
            }
        }
      case Zval(ttype) =>
        (nodes
           .NewLiteral()
           .typeFullName("zval")
           .code(s"$ttype")
           .order(order),
         s"zval($value)")
      case ByteCodeKeyword(value) =>
        (nodes
           .NewLiteral()
           .typeFullName("ByteCodeKeyword")
           .order(order)
           .code(s"$value"),
         s"($value)")
      case ByteCodePlaceIndicator(value) =>
        (nodes
           .NewLiteral()
           .typeFullName("ByteCodePlaceIndicator")
           .order(order)
           .code(s"$value"),
         s"$value")
      case x =>
        throw new RuntimeException(
          s"unexpected matching in control flow block $x")
    }
  }

}
