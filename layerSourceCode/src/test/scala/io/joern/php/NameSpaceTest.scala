package io.joern.php

import io.joern.TraversalUtils
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class NameSpaceTest extends AnyWordSpec with Matchers with TraversalUtils {

  override val fixture: CpgFromCodeTestFixture = CpgFromCodeTestFixture(
    """
      |namespace test\test\test;
      |use otherNameSpace\test as other;
      |use function name\space\func as func;
      |use const name\space\constant;
      |name\space\function();
      |\PleskX\Api\Client::RESPONSE_FULL;
      |""".stripMargin)

  "cpg" should {
    "have a single file" in {
      fixture.cpg.file.l.length shouldBe 1
    }
    "have a single name space with a single child with 5 children" in {
      fixture.cpg
        .namespaceBlock("test\\test\\test".replace("\\", "\\\\"))
        .l
        .length shouldBe 1
      fixture.cpg
        .namespaceBlock("test\\test\\test".replace("\\", "\\\\"))
        .astChildren
        .l
        .length shouldBe 1
      fixture.cpg
        .namespaceBlock("test\\test\\test".replace("\\", "\\\\"))
        .astChildren
        .astChildren
        .l
        .length shouldBe 5
    }
    "has three use calls" in {
      fixture.cpg.call("use").l.length shouldBe 3
    }
    "the first one should be a regular with alias" in {
      val call =
        fixture.cpg.call("use").l.sortWith((a, b) => a.order < b.order).head
      call.astChildren.l.length shouldBe 3
      val children = call.astChildren.l.sortWith((a, b) => a.order < b.order)
      children.head match {
        case child: nodes.Literal =>
          child.code shouldBe "general"
          child.typeFullName shouldBe "keyword"
      }
      children(1) match {
        case child: nodes.Literal =>
          child.code shouldBe "otherNameSpace\\test"
          child.typeFullName shouldBe "namespaceName"
      }
      children(2) match {
        case child: nodes.Identifier =>
          child.name shouldBe "other"
      }
    }

    "the second one should be a function with alias" in {
      val call =
        fixture.cpg.call("use").l.sortWith((a, b) => a.order < b.order).apply(1)
      call.astChildren.l.length shouldBe 3
      val children = call.astChildren.l.sortWith((a, b) => a.order < b.order)
      children.head match {
        case child: nodes.Literal =>
          child.code shouldBe "function"
          child.typeFullName shouldBe "keyword"
      }
      children(1) match {
        case child: nodes.Literal =>
          child.code shouldBe "name\\space\\func"
          child.typeFullName shouldBe "namespaceName"
      }
      children(2) match {
        case child: nodes.Identifier =>
          child.name shouldBe "func"
      }
    }

    "the third one should be a const without alias" in {
      val call =
        fixture.cpg.call("use").l.sortWith((a, b) => a.order < b.order).apply(2)
      call.astChildren.l.length shouldBe 2
      val children = call.astChildren.l.sortWith((a, b) => a.order < b.order)
      children.head match {
        case child: nodes.Literal =>
          child.code shouldBe "const"
          child.typeFullName shouldBe "keyword"
      }
      children(1) match {
        case child: nodes.Literal =>
          child.code shouldBe "name\\space\\constant"
          child.typeFullName shouldBe "namespaceName"
      }
    }

    "have a single call to name\\space\\function" in {
      fixture.cpg
        .call("name\\space\\function".replace("\\", "\\\\"))
        .l
        .length shouldBe 1
    }

    "have a single const fetch" in {
      val const = fixture.cpg.call("classConstFetch").l.head
      const.name shouldBe "classConstFetch"
      const.astChildren.l.length shouldBe 2
      const.astChildren.order(0).l.head match {
        case node: nodes.Literal =>
          node.code shouldBe "\\PleskX\\Api\\Client"
          node.typeFullName shouldBe "fullyQualifiedName"
      }
      const.astChildren.order(1).head match {
        case node: nodes.Identifier =>
          node.name shouldBe "RESPONSE_FULL"
      }
    }
  }
}
