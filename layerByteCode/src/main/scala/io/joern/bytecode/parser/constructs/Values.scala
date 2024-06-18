package io.joern.bytecode.parser.constructs

import spray.json._
import spray.json.{DefaultJsonProtocol, RootJsonFormat}
import spray.json.DefaultJsonProtocol._

sealed trait Value

case class Variable(name: String, tmp: Boolean, reference: Boolean = false)
    extends Value

case class ArrayValue(content: Option[List[ArrayKeyValuePair]]) extends Value

case class AssignOpLiteral(value: String) extends Value

case class StringLiteral(value: String) extends Value

case class IntegerLiteral(value: Long) extends Value

case class FloatLiteral(value: Float) extends Value

case class BooleanLiteral(value: Boolean) extends Value

case class TryCatchLiteral(value: Int) extends Value

case class Null() extends Value

case class Zval(ttype: Int) extends Value

case class ByteCodeKeyword(value: String) extends Value

case class ByteCodePlaceIndicator(value: String) extends Value

case class ByteCodeConstructor() extends Value

case class KeyValuePair(key: Either[Int, String], value: String) extends Value

case class ArrayKeyValuePair(key: Either[Int, String], value: Value)
    extends Value

case class DefaultKeyValuePair(value: String) extends Value

object MyJsonProtocol extends DefaultJsonProtocol {

  implicit object keyFormat extends RootJsonFormat[Either[Int, String]] {
    override def write(obj: Either[Int, String]): JsValue = obj match {
      case Left(value)  => value.toString.toJson
      case Right(value) => value.toJson
    }

    override def read(json: JsValue): Either[Int, String] = ???
  }

  implicit object valueFormat extends RootJsonFormat[Value] {
    override def write(obj: Value): JsValue = obj match {
      case Variable(name, tmp, ref) =>
        (if (ref || tmp) s"$name" else "CV($" + name + ")").toJson
      case av: ArrayValue                => arrayValueFormat.write(av)
      case AssignOpLiteral(value)        => value.toJson
      case StringLiteral(value)          => value.toJson
      case IntegerLiteral(value)         => value.toJson
      case FloatLiteral(value)           => value.toJson
      case BooleanLiteral(value)         => value.toJson
      case TryCatchLiteral(value)        => value.toJson
      case Null()                        => JsNull
      case Zval(ttype)                   => ttype.toJson
      case ByteCodeKeyword(value)        => value.toJson
      case ByteCodePlaceIndicator(value) => value.toJson
      case ByteCodeConstructor()         => JsString("CONSTRUCTOR")
      case KeyValuePair(key, value) => {
        key -> value
      }.toJson
      case _: ArrayKeyValuePair       => ???
      case DefaultKeyValuePair(value) => value.toJson
    }

    override def read(json: JsValue): Value = ???
  }

  implicit object arrayValueFormat extends RootJsonFormat[ArrayValue] {
    override def write(obj: ArrayValue): JsValue = {
      obj.content match {
        case Some(value) =>
          value
            .flatMap(x =>
              Map {
                x.key -> x.value
            })
            .toMap
            .toJson
        case None => "PHP2CPG-NESTED-ARRAY-LIMITATION".toJson
      }
    }

    override def read(json: JsValue): ArrayValue = ???
  }
}
