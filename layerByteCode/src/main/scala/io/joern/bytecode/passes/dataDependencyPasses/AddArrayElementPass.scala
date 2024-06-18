package io.joern.bytecode.passes.dataDependencyPasses

import io.joern.reporting.{ReportableError, Reporting}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier, Method}
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool, ParallelCpgPass}
import io.shiftleft.semanticcpg.language._

import scala.collection.mutable.{ListBuffer, Map => MMap}

class AddArrayElementPass(methods: Seq[Method],
                          cpg: Cpg,
                          keyPool: IntervalKeyPool)
    extends ParallelCpgPass[Method](
      cpg: Cpg,
      keyPools = Some(keyPool.split(methods.size)))
    with Reporting {

  override val name = "AddArrayElementPass"

  override def partIterator: Iterator[Method] = methods.iterator

  override def runOnPart(part: Method): Iterator[DiffGraph] = {
    implicit val diffGraph: DiffGraph.Builder = DiffGraph.newBuilder
    try {
      generateTmpVarMap(
        part.ast.filter(_.isInstanceOf[Call]).toList.map(_.asInstanceOf[Call]))
        .foreach {
          case (variable, nodes) =>
            assert(
              nodes.count(_.name == "INIT_ARRAY") == 1,
              s"there must only be a single INIT_ARRAY for $variable but there are ${nodes
                .count(_.code == "INIT_ARRAY")}")
            nodes.sortBy(_.astParent.order).sliding(2, 1).foreach {
              case _ :: Nil => // if we only have INIT_ARRAY nothing needs to be done
              case from :: to :: Nil =>
                diffGraph.addEdge(from,
                                  to,
                                  EdgeTypes.REACHING_DEF,
                                  List(("VARIABLE", variable)))
            }
        }
    } catch {
      case err: ReportableError => reportError(err)
      case thr: Throwable =>
        reportError(part.filename,
                    "NA",
                    part.fullName,
                    "NA",
                    thr.toString + thr.getStackTrace.mkString("\n"),
                    thr)
    }
    Iterator(diffGraph.build())
  }

  private def generateTmpVarMap(nodes: List[Call]): Map[String, List[Call]] = {
    val tmpVarMap: MMap[String, ListBuffer[Call]] = MMap()
    nodes
      .filter(_.name == "INIT_ARRAY")
      .foreach { // iterate over all init array calls
        initArray: Call =>
          // get the parent node which has to be an = assign
          val assignment = initArray.astParent.asInstanceOf[Call]
          // ensure that this is the case
          assert(assignment.name == "=",
                 "the parent of INIT_ARRAY has to be an = assign")
          // get the name of the variable it is assigned to
          val tmpVariable = assignment.astChildren
            .sortBy(_.order)
            .head
            .asInstanceOf[Identifier]
            .name
          // ensure that our assumption of it being a tmp variable holds
          assert(
            tmpVariable.substring(0, 1) == "T",
            s"the assigned to variable should be a temporary internal one but is $name")
          assert(!tmpVarMap.contains(tmpVariable),
                 "the variable must not have been used previously")
          tmpVarMap.addOne(tmpVariable -> ListBuffer(initArray))
      }
    nodes
      .filter(_.name == "ADD_ARRAY_ELEMENT")
      .foreach { // iterate over all add array element calls
        addArrayElement: Call =>
          // get the parent node which has to be an = assign
          val assignment = addArrayElement.astParent.asInstanceOf[Call]
          // ensure that this is the case
          assert(assignment.name == "=",
                 "the parent of INIT_ARRAY has to be an = assign")
          // get the name of the variable it is assigned to
          val tmpVariable = assignment.astChildren
            .sortBy(_.order)
            .head
            .asInstanceOf[Identifier]
            .name
          // ensure that our assumption of it being a tmp variable holds
          assert(tmpVariable.substring(0, 1) == "T",
                 "the assigned to variable should be a temporary internal one")
          assert(tmpVarMap.contains(tmpVariable),
                 "the variable must already exist")
          tmpVarMap(tmpVariable).addOne(addArrayElement)
      }
    tmpVarMap.map(pair => pair._1 -> pair._2.toList).toMap
  }

}
