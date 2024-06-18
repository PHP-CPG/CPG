package io.joern.bytecode.util.unittesting

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.CfgNode
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, nodes}
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.dotextension.ImageViewer
import io.shiftleft.semanticcpg.passes.controlflow.cfgcreation.Cfg.CfgEdgeType
import overflowdb.traversal.{NodeOps, Traversal}

import scala.jdk.CollectionConverters._
import scala.sys.process.Process
import scala.util.Try

/** NOTE
  *
  * We do not want to use regexp matching. Regexp matching is a bad fit for unit testing as it may lead to FP (or FN)
  * if the given code contains regexp characters or the regexp is over inclusive.
  * For testing purposes only exact code matching shall be used.
  *
  * Some people, when confronted with a problem, think : “I know, I'll use regular expressions.”
  * Now they have two problems.
  *
  */
abstract class AbstractCpgTestFixture {

  implicit var cpg: Cpg

  def codeToNode(parentCall: Option[String] = None)(
      implicit method: String): Map[String, nodes.CfgNode] = {
    var retSet = Set[String]()
    var dupl = Set[String]()
    assert(
      cpg.method.fullNameExact(method).hasNext,
      s"the method $method does not exist only ${cpg.method.map(_.fullName).l}")
    val ret: Map[String, nodes.CfgNode] = parentCall match {
      case None =>
        assert(
          cpg.method.fullNameExact(method).hasNext,
          s"method $method does not exist there are only ${cpg.method.map(_.name).l}")
        cpg.method
          .fullNameExact(method)
          .head
          .ast
          .isCfgNode
          .l
          .map { node =>
            if (retSet.contains(node.code)) {
              dupl = dupl + node.code
            } else {
              retSet = retSet + node.code
            }
            node.code -> node
          }
          .toMap
      case Some(parentCode) =>
        val nodes = cpg.method.fullNameExact(method).ast.isCfgNode.l
        assert(nodes.nonEmpty, s"there are no cfg children for $method")
        val parent: Traversal[CfgNode] = nodes.codeExact(parentCode)
        assert(
          parent.hasNext,
          s"there is no node '$parentCode' for method $method. Only:\n ${nodes.map(_.code).mkString("\n")}")
        parent.ast.isCfgNode.l.map { node =>
          if (retSet.contains(node.code)) {
            dupl = dupl + node.code
          } else {
            retSet = retSet + node.code
          }
          node.code -> node
        }.toMap
    }
    ret
  }

  def expectedCfg(pairs: (String, CfgEdgeType)*)(
      implicit method: String): Set[String] = {
    expectedCfg(None, pairs: _*)
  }

  def expectedCfg(parentCall: String, pairs: (String, CfgEdgeType)*)(
      implicit method: String): Set[String] = {
    expectedCfg(Some(parentCall), pairs: _*)
  }

  private def expectedCfg(
      parentCall: Option[String],
      pairs: (String, CfgEdgeType)*)(implicit method: String): Set[String] = {
    pairs.map {
      case (code, _) =>
        val map = codeToNode(parentCall)
        map
          .getOrElse(
            code,
            throw new RuntimeException(
              s"$code with parent $parentCall is unknown for method $method. Map contained #${map.size} nodes :${map.keys
                .mkString("|")}")
          )
          .start
          .code
          .head
    }.toSet
  }

  def expectedDdg(pairs: (String, String)*)(
      implicit method: String): Set[(String, String)] = {
    expectedDdg(None, pairs: _*)
  }

  def expectedDdg(parentCall: String, pairs: (String, String)*)(
      implicit method: String): Set[(String, String)] = {
    expectedDdg(Some(parentCall), pairs: _*)
  }

  private def expectedDdg(parentCall: Option[String], pairs: (String, String)*)(
      implicit method: String): Set[(String, String)] = {
    pairs.map {
      case (code, variable) =>
        val map = codeToNode(parentCall)
        assert(map.contains(code), s"there is no node with code $code")
        val node = map(code).start.code
        //assert(node.nonEmpty, s"there is no $pairs for parent $parentCall")
        (node.head, variable)
    }.toSet
  }

  def cfgSuccOf(code: String)(implicit method: String): Set[String] = {
    cfgSuccOf(None, code)
  }

  def cfgSuccOf(parentCall: String, code: String)(
      implicit method: String): Set[String] = {
    cfgSuccOf(Some(parentCall), code)
  }

  private def cfgSuccOf(parentCall: Option[String], code: String)(
      implicit method: String): Set[String] = {
    val map = codeToNode(parentCall)
    val buff = map
      .getOrElse(
        code,
        throw new RuntimeException(
          s"did not find code >>$code<< of parent call $parentCall in method $method, available: (${map.keys})"))
      ._cfgOut
      .asScala
      .map(_.asInstanceOf[nodes.CfgNode])
      .toSet
    buff.map[String](_.code)
  }

  def cfgPredsOf(code: String)(implicit method: String): Set[String] = {
    cfgPredsOf(None, code)
  }

  def cfgPredsOf(parentCall: String, code: String)(
      implicit method: String): Set[String] = {
    cfgPredsOf(Some(parentCall), code)
  }

  private def cfgPredsOf(parentCall: Option[String], code: String)(
      implicit method: String): Set[String] = {
    val map = codeToNode(parentCall)
    map(code)._cfgIn.asScala
      .map(_.asInstanceOf[nodes.CfgNode])
      .toSet
      .map[String](_.code)
  }

  def ddgSuccOf(code: String)(
      implicit method: String): Set[(String, String)] = {
    ddgSuccOf(None, code)
  }

  def ddgSuccOf(parentCall: String, code: String)(
      implicit method: String): Set[(String, String)] = {
    ddgSuccOf(Some(parentCall), code)
  }

  private def ddgSuccOf(parentCall: Option[String], code: String)(
      implicit method: String): Set[(String, String)] = {
    val map = codeToNode(parentCall)
    map
      .getOrElse(
        code,
        throw new RuntimeException(
          s"the code segment $code of method $method with specific parent $parentCall does not exist"))
      .outE(EdgeTypes.REACHING_DEF)
      .asScala
      .map(edge =>
        (edge.inNode().asInstanceOf[nodes.CfgNode].code,
         edge.property("VARIABLE").toString))
      .toSet
  }

  def ddgPredOf(code: String)(
      implicit method: String): Set[(String, String)] = {
    ddgPredOf(None, code)
  }

  def ddgPredOf(parentCall: String, code: String)(
      implicit method: String): Set[(String, String)] = {
    ddgPredOf(Some(parentCall), code)
  }

  private def ddgPredOf(parentCall: Option[String], code: String)(
      implicit method: String): Set[(String, String)] = {
    val map = codeToNode(parentCall)
    map
      .getOrElse(
        code,
        throw new RuntimeException(
          s"the code segment $code of method $method with specific parent $parentCall does not exist"))
      .inE(EdgeTypes.REACHING_DEF)
      .asScala
      .map(edge =>
        (edge.outNode().asInstanceOf[nodes.CfgNode].code,
         edge.property("VARIABLE").toString))
      .toSet
  }

  implicit val viewer: ImageViewer = (pathStr: String) =>
    Try {
      Process(Seq("xdg-open", pathStr)).!!
  }

}
