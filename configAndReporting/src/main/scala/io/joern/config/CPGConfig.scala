package io.joern.config

import com.typesafe.config.ConfigFactory
import de.halcony.argparse.{FlagValue, OptionalValue, Parser}
import io.joern.reporting.LogEvaluation

import java.io.File
import scala.jdk.CollectionConverters.ListHasAsScala
import scala.sys.exit

case class PHPInterpreter(interpreter: String, phpini: String)

case class CPGConfig(layer: Option[String],
                     output: String,
                     rootFolder: String,
                     files: List[String],
                     strictLinking: Boolean,
                     strictParsing: Boolean,
                     php7: PHPInterpreter,
                     php8: PHPInterpreter,
                     phpversion: String,
                     report: Boolean,
                     forced: Boolean,
                     passes: Set[String],
                     verbose: Boolean)

object CPGConfig {

  private val defaultConfigFile = "./main.conf"

  private val parser = Parser("PHP Cpg Creator", "create a cpg from PHP")
    .addOptional("config",
                 "c",
                 "config",
                 Some(defaultConfigFile),
                 "the config file to use")
    .addPositional("rootFolder", "the root folder of the project")
    .addOptional(
      "output",
      "o",
      "output",
      None,
      s"the destination file into which to store the cpg (def:<config>)")
    .addFlag("forced", "f", "forced", "when set files are simply overwritten")
    .addFlag("strictParsing",
             "s",
             "strictParsing",
             "if set each project file has to be parsed successfully")
    .addFlag("permissiveLinking",
             "l",
             "permissiveLinking",
             "linking strategy for call graph set to permissive")
    .addOptional("fileEndings",
                 "e",
                 "endings",
                 None,
                 "the file endings included for analysis (def:<config>)")
    .addFlag("verbose", "v", "verbose", "whether or not CPG log spam is active")
    .addSubparser(LogEvaluation.parser)
    .addSubparser(Parser("bytecode", "create a bytecode CPG")
      .addDefault[String]("layer", "bytecode")
      .addPositional("version", "the php version to use {7,8}")
      .addOptional(
        "php",
        "p",
        "php",
        None,
        "the php interpreter to use to create bytecode (def:<config>)")
      .addOptional("phpini", "i", "phpini", None, "the php.ini (def:<config>)"))

  def initializeConfig(argsv: Array[String]): CPGConfig = {
    val pargs = try {
      parser.parse(argsv)
    } catch {
      case _: de.halcony.argparse.ParsingException =>
        exit(1)
      case x: Throwable =>
        println(s"unable to initialize: ${x.getMessage}")
        exit(1)
    }
    if (pargs.getValue[String]("layer") == "log-profiler") {
      LogEvaluation.profiler(pargs)
      return null
    }
    val config = pargs.getValue[String]("config")
    val preConfig = initializeConfig(config)
    val interpreter: Option[PHPInterpreter] =
      (pargs.get[OptionalValue[String]]("php").value,
       pargs.get[OptionalValue[String]]("phpini").value) match {
        case (Some(interpreter), Some(ini)) =>
          Some(PHPInterpreter(interpreter, ini))
        case (Some(interpreter), None) =>
          pargs.getValue[String]("version") match {
            case "7" => Some(PHPInterpreter(interpreter, preConfig.php7.phpini))
            case "8" => Some(PHPInterpreter(interpreter, preConfig.php8.phpini))
          }
        case (None, Some(ini)) =>
          pargs.getValue[String]("version") match {
            case "7" => Some(PHPInterpreter(preConfig.php7.interpreter, ini))
            case "8" => Some(PHPInterpreter(preConfig.php8.interpreter, ini))
          }
        case (None, None) => None
      }
    CPGConfig(
      Some(pargs.getValue[String]("layer")),
      pargs
        .get[de.halcony.argparse.OptionalValue[String]]("output")
        .value
        .getOrElse(preConfig.output),
      pargs.getValue[String]("rootFolder"),
      pargs
        .get[OptionalValue[String]]("fileEndings")
        .value
        .getOrElse(preConfig.files.mkString(","))
        .split(",")
        .toList,
      if (pargs.get[FlagValue]("permissiveLinking").provided) {
        !pargs.get[FlagValue]("permissiveLinking").value
      } else {
        preConfig.strictLinking
      },
      if (pargs.get[FlagValue]("strictParsing").provided) {
        pargs.get[FlagValue]("strictParsing").value
      } else {
        preConfig.strictParsing
      },
      interpreter.getOrElse(preConfig.php7),
      interpreter.getOrElse(preConfig.php8),
      pargs.getValue[String]("version"),
      preConfig.report,
      pargs.get[FlagValue]("forced").value,
      preConfig.passes,
      pargs.get[FlagValue]("verbose").value
    )
  }

  def initializeConfig(configFile: String = defaultConfigFile): CPGConfig = {
    val conf = ConfigFactory.parseFile(new File(configFile))
    CPGConfig(
      None,
      conf.getString("cpg.output"),
      "CONFIGHASNONE",
      conf.getStringList("cpg.files").asScala.toList,
      conf.getBoolean("cpg.strictLinking"),
      conf.getBoolean("cpg.strictParsing"),
      PHPInterpreter(conf.getString("cpg.php.7.interpreter"),
                     conf.getString("cpg.php.7.phpini")),
      PHPInterpreter(conf.getString("cpg.php.8.interpreter"),
                     conf.getString("cpg.php.8.phpini")),
      "CONFIGHASNONE",
      conf.getBoolean("cpg.report"),
      conf.getBoolean("cpg.forced"),
      verbose = false,
      passes = conf
        .getStringList("cpg.activePasses")
        .asScala
        .map(_.toLowerCase)
        .toSet
    )
  }

}
