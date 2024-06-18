package io.joern.bytecode.parser.php8

import fastparse.NoWhitespace._
import fastparse._
import io.joern.bytecode.parser.FileParser
import io.joern.bytecode.parser.constructs.MethodDefinitionPair
import io.joern.bytecode.parser.php8.MethodDefinition.getFullMethodDefinitionBlock
import io.joern.reporting.Reporting

import java.io.{ByteArrayOutputStream, PrintWriter, File => JFile}
import java.nio.charset.StandardCharsets
import java.text.SimpleDateFormat
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.sys.process._

object FileParser8 {

  trait PHPParseError extends Throwable {
    def getMessage: String
  }

  case class PHPSyntaxError(msg: String) extends PHPParseError {
    override def getMessage: String = s"Syntax Error: $msg"
  }

  case class PHPFatalError(msg: String) extends PHPParseError {
    override def getMessage: String = s"PHP Error: $msg"
  }

  case class PHPWarning(msg: String) extends PHPParseError {
    override def getMessage: String = s"PHP Warn: $msg"
  }

  case class BytecodeParseError(msg: String) extends Throwable {
    override def getMessage: String = msg
  }

  val PREPROCESSING = false
  val MAX_BYTECODE_OUTPUT = 1000

  def parsePossibleParseError[_: P]: P[String] =
    P("PHP Parse error: " ~ AnyChar.rep.!)

  def parsePossibleFatalError[_: P]: P[String] =
    P("PHP Fatal error: " ~ AnyChar.rep.!)

  def parsePossibleWarning[_: P]: P[String] =
    P("PHP Warning: " ~ AnyChar.rep.!)

  def parseLastLine[_: P]: P[Unit] =
    P("No syntax errors " ~/ "detected in " ~/ AnyChar.rep)

  def parseByteCodeDump[_: P]: P[Seq[MethodDefinitionPair]] =
    P(("\n" ~ getFullMethodDefinitionBlock).rep ~ End)

  def parsePHPEventsWithoutThrowing[_: P]: P[Unit] =
    P((parsePossibleParseError | parsePossibleFatalError | parsePossibleWarning | "\n").rep)

  def parsePHPEvents(input: String) = {
    fastparse.parse(input, parsePossibleParseError(_)) match {
      case Parsed.Success(errorMessage, _) => throw PHPSyntaxError(errorMessage)
      case _                               =>
    }
    fastparse.parse(input, parsePossibleFatalError(_)) match {
      case Parsed.Success(errorMessage, _) => throw PHPFatalError(errorMessage)
      case _                               =>
    }
    fastparse.parse(input, parsePossibleWarning(_)) match {
      case Parsed.Success(message, _) => throw PHPWarning(message)
      case _                          =>
    }
  }

  def actualParse(input: String,
                  file: String,
                  strict: Boolean = true): Seq[MethodDefinitionPair] = {
    //Some(new PrintWriter("input_dump.txt")).foreach { p =>
    //  p.write(input); p.close()
    //}
    parsePHPEvents(input)
    try {
      fastparse.parse(input, parseByteCodeDump(_)) match {
        case Parsed.Success(value, length) =>
          if (length != input.length) {
            val remainder = input.substring(length, input.length)
            if (strict) {
              parsePHPEvents(remainder)
            } else {
              fastparse.parse(remainder, parsePHPEventsWithoutThrowing(_)) match {
                case Parsed.Success(_, length) =>
                  if (length != remainder.length) {
                    throw BytecodeParseError(
                      s"remaining code is:\n>>${remainder.substring(length, remainder.length.min(length + MAX_BYTECODE_OUTPUT))} [...]<<")
                  }
                case x: Parsed.Failure =>
                  val t = x.trace(true)
                  throw BytecodeParseError(s"${x.toString()}\n${t.longMsg}")
              }
            }
          }
          value
        case x: Parsed.Failure =>
          val t = x.trace(true)
          throw BytecodeParseError(s"${x.toString()}\n${t.longMsg}")
      }
    } catch {
      case x: Throwable =>
        throw BytecodeParseError(
          s"In file $file encountered:${x.toString}\n${Reporting.stringifyErrorStack(x)}\n")
    }
  }

  def setTimestamp(filePath: String): Boolean = {
    val file = new JFile(filePath)
    val date = new SimpleDateFormat("MM/dd/yyyy")
    val last = date.parse("10/03/1990")
    file.setLastModified(last.getTime)
  }

  def generatePhpByteCodeDump(filePath: String,
                              phpInterpreter: String,
                              phpini: String): String = {
    val stderrStream = new ByteArrayOutputStream()
    val stdoutStream = new ByteArrayOutputStream()
    val stdoutWriter =
      new PrintWriter(stdoutStream, true, StandardCharsets.UTF_8)
    val stderrWriter =
      new PrintWriter(stderrStream, true, StandardCharsets.UTF_8)
    val command = s"$phpInterpreter -c $phpini -d opcache.enable_cli=1 -d opcache.opt_debug_level=0x50000 -d opcache.log_verbosity_level=0 --syntax-check ${"\""}" + filePath + s"${"\""}"
    command.!(ProcessLogger(stdoutWriter.println, stderrWriter.println))
    stderrWriter.close()
    stdoutWriter.close()
    val ret = stderrStream.toString
    ret
  }

  def parseFromFile(file: java.io.File,
                    phpInterpreter: String,
                    phpini: String,
                    strict: Boolean = true): Seq[MethodDefinitionPair] = {
    val string =
      generatePhpByteCodeDump(file.getPath, phpInterpreter, phpini: String)
    actualParse(string, file.getPath, strict)
  }

  def parseFromByteCodeDump(
      dump: String,
      strict: Boolean = true): Seq[MethodDefinitionPair] = {
    actualParse(dump, "<parsed from bytecode dump>", strict)
  }
}

class FileParser8(files: Seq[JFile],
                  phpInterpreter: String,
                  phpini: String,
                  strict: Boolean)
    extends FileParser {

  val name = "FileParser"

  def run(): List[Seq[MethodDefinitionPair]] = {
    implicit val ec: ExecutionContext = ExecutionContext.global

    val list_of_futures: Seq[Future[Option[Seq[MethodDefinitionPair]]]] =
      files.map(file =>
        Future {
          var to_return: Option[Seq[MethodDefinitionPair]] = None
          withErrorReporting() {
            FileParser8
              .parseFromFile(file, phpInterpreter, phpini, strict) match {
              case Nil =>
                reportWarning(file.getPath,
                              "",
                              "",
                              "",
                              "no methods extracted - usually a bad sign")
              case x => to_return = Some(x)
            }
          }
          to_return
      })
    val nested_res =
      Await.result(Future.sequence(list_of_futures), Duration.Inf)
    nested_res.flatten.toList
  }
}
