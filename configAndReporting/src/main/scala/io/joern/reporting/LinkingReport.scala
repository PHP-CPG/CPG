package io.joern.reporting

case class LinkingReport(internalFunctions: List[String],
                         linkedReferencedFunctions: List[String],
                         unlinkedDefinedFunctions: List[String],
                         linkedReferencedMethods: List[String],
                         unlinkedReferencedMethods: List[String])
