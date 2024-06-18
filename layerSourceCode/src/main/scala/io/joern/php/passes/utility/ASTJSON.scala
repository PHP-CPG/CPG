package io.joern.php.passes.utility

object ASTJSON {

  def format(t: Any, i: Int = 0): String = t match {

    case o: Map[String, Any] =>
      o.map {
          case (k, v) =>
            " " * (i + 1) + k + " : " + format(v, i + 1)
        }
        .mkString("{\n", "\n", "\n" + " " * i + "}")
    case a: List[_] =>
      a.map { e =>
          " " * (i + 1) + format(e, i + 1)
        }
        .mkString("[\n", "\n", "\n" + " " * i + "]")

    case x => s"$x"
  }

  def missingKey(key: String, json: Any): Unit = {
    throw new RuntimeException(s"key $key does not exist in ${format(json)}")
  }

  def getLineStart(json: Map[String, Any]): Option[Integer] = {
    json.getOrElse("startLine", missingKey("startLine", json)) match {
      case x: Double => Some(x.toInt)
    }
  }

  def getLineEnd(json: Map[String, Any]): Option[Integer] = {
    json.getOrElse("endLine", missingKey("startLine", json)) match {
      case x: Double => Some(x.toInt)
    }
  }

  def getJsonObject(attr: String,
                    json: Map[String, Any]): Option[Map[String, Any]] = {
    json.getOrElse(attr, missingKey(attr, json)) match {
      case x: Map[String, Any] => Some(x)
      case x =>
        Option(x) match {
          case None => None
          case Some(_) =>
            throw new RuntimeException("there should be only a None value")
        }
    }
  }

  def getJsonAtom[T](attr: String, json: Map[String, Any]): Option[T] = {
    json.getOrElse(attr, missingKey(attr, json)) match {
      case x: T => Option(x)
      case x =>
        Option(x) match {
          case None => None
          case Some(_) =>
            throw new RuntimeException("there should be only a None value")
        }
    }
  }

  def getJsonDouble(attr: String, json: Map[String, Any]): Option[Double] = {
    json.getOrElse(attr, missingKey(attr, json)) match {
      case x: Double => Some(x)
      case x =>
        Option(x) match {
          case None => None
          case Some(_) =>
            throw new RuntimeException("there should be only a None value")
        }
    }
  }

  def getJsonList(attr: String,
                  json: Map[String, Any]): List[Map[String, Any]] = {
    json.getOrElse(attr, missingKey(attr, json)) match {
      case x: List[Map[String, Any]] => x
      case y =>
        println(format(json))
        throw new RuntimeException(
          s"the requested attribute $attr is not a list but ${y.getClass.toString}")
    }
  }

}
