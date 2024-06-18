package io.joern

import io.joern.php.passes.{AstCreationPass, MetaDataPass}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.KeyPoolCreator
import io.shiftleft.x2cpg.{SourceFiles, X2Cpg}
import scopt.OptionParser

object PhpToCpg {

  def main(args: Array[String]): Unit = {
    parseConfig(args).foreach { config =>
      val cpg = new PhpToCpg().run(
        SourceFiles.determine(Set(config.inputPath), Set(".php")).toSet,
        config.outputPath)
      cpg.close()
    }
  }

  final case class Config(inputPath: String = "-1",
                          outputPath: String = "cpg.bin")

  def parseConfig(args: Array[String]): Option[Config] = {
    new OptionParser[Config](classOf[PhpToCpg].getSimpleName) {
      arg[String]("base-path")
        .text("base directory of the PHP project")
        .action((x, c) => c.copy(inputPath = x))
      opt[String]("output")
        .abbr("o")
        .text("output filename")
        .action((x, c) => c.copy(outputPath = x))
    }.parse(args, Config())
  }

}

class PhpToCpg {

  def run(fileNames: Set[String], outputPath: String): Cpg = {
    val keyPools = KeyPoolCreator.obtain(2)
    val cpg = X2Cpg.newEmptyCpg(Some(outputPath))
    new MetaDataPass(cpg, keyPools.head).createAndApply()
    new AstCreationPass(fileNames.toSeq, cpg, keyPools.head)
      .createAndApply()
    cpg
  }

}
