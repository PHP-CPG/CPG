package io.joern.bytecode.parser

import io.joern.bytecode.parser.constructs.MethodDefinitionPair
import io.joern.reporting.Reporting

trait FileParser extends Reporting {

  def run(): List[Seq[MethodDefinitionPair]]

}
