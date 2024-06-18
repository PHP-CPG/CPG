package io.joern.reporting

import scala.collection.mutable.ListBuffer

trait Linking {

  private val internalFunctions: ListBuffer[String] = new ListBuffer[String]()
  private val linkedReferencedFunctions: ListBuffer[String] =
    new ListBuffer[String]()
  private val unlinkedReferencedFunctions: ListBuffer[String] =
    new ListBuffer[String]()
  private val linkedReferencedMethods: ListBuffer[String] =
    new ListBuffer[String]()
  private val unlinkedReferencedMethod: ListBuffer[String] =
    new ListBuffer[String]()

  def unlinkedInternalFunction(str: String): Unit =
    internalFunctions.synchronized {
      internalFunctions.addOne(str)
    }

  def linkedFunction(str: String): Unit =
    linkedReferencedFunctions.synchronized {
      linkedReferencedFunctions.addOne(str)
    }

  def unableToLinkFunction(str: String): Unit =
    unlinkedReferencedFunctions.synchronized {
      unlinkedReferencedFunctions.addOne(str)
    }

  def linkedMethod(str: String): Unit = linkedReferencedMethods.synchronized {
    linkedReferencedMethods.addOne(str)
  }

  def unableToLinkMethod(str: String): Unit =
    unlinkedReferencedMethod.synchronized {
      unlinkedReferencedMethod.addOne(str)
    }

  def getLinkingReport: LinkingReport = LinkingReport(
    internalFunctions.toSet.toList,
    linkedReferencedFunctions.toSet.toList,
    unlinkedReferencedFunctions.toSet.toList,
    linkedReferencedMethods.toSet.toList,
    unlinkedReferencedMethod.toSet.toList
  )

}
