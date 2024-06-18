package io.joern.bytecode.passes.utility

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.Traversal

import java.io.File

class FileNotFoundException(file: String, results: List[nodes.AstNode])
    extends Exception {

  override def toString: String = {
    s"Identifying unique file $file lead to results $results"
  }

}

class NamespaceNotFoundException(namespace: String,
                                 file: Option[nodes.File],
                                 results: List[nodes.AstNode])
    extends Exception {

  override def getMessage: String = {
    val str = new StringBuilder
    str.append(s"Identifying unique namespace $namespace")
    str.append(file match {
      case Some(file) => s" in specified file $file"
      case None       => ""
    })
    str.append(s" lead to $results")
    str.toString()
  }

}

class TypeDeclNotFoundException(typeDecl: String,
                                namespace: Option[nodes.NamespaceBlock],
                                file: Option[nodes.File],
                                results: List[nodes.AstNode])
    extends Exception {

  override def getMessage: String = {
    val str = new StringBuilder
    str.append(s"Identifying unique TypeDecl $typeDecl")
    str.append(namespace match {
      case Some(namespace) => s" in specified namespace ${namespace.name}"
      case None            => ""
    })
    str.append(file match {
      case Some(file) => s" in specified file ${file.name}"
      case None       => ""
    })
    str.append(s" lead to results $results")
    str.toString()
  }

}

class MethodNotFoundException(method: String,
                              typeDecl: Option[nodes.TypeDecl],
                              namespace: Option[nodes.NamespaceBlock],
                              file: Option[nodes.File],
                              results: List[nodes.AstNode])
    extends Exception {

  override def getMessage: String = {
    val str = new StringBuilder
    str.append(s"Identifying unique method $method")
    str.append(typeDecl match {
      case Some(typeDecl) => s" in specified TypeDecl ${typeDecl.name}"
      case None           => ""
    })
    str.append(namespace match {
      case Some(namespace) => s" in specified namespace ${namespace.name}"
      case None            => ""
    })
    str.append(file match {
      case Some(file) => s" in specified file ${file.name}"
      case None       => ""
    })
    str.append(s" lead to results $results")
    str.append(s" with Names: ${results
      .filter(_.isInstanceOf[Method])
      .map(_.asInstanceOf[Method])
      .map(x => (x.name, x.fullName, x.lineNumber, x.lineNumberEnd))}")
    str.append(s" amount of unique results: ${results.toSet.size}")
    str.toString()
  }

}

object MethodIdentification {

  def getUniqueMethod(
      file: Option[String],
      namespace: Option[String],
      typeDeclName: Option[String],
      name: String,
      start: Option[Int] = None,
      end: Option[Int] = None)(implicit cpg: Cpg): nodes.Method = {
    val fileNode: Option[nodes.File] = file match {
      case Some(filePath) => Some(getUniqueFile(filePath))
      case None           => None
    }
    val namespaceNode: Option[nodes.NamespaceBlock] = namespace match {
      case Some(namespaceName) =>
        Some(getUniqueNamespace(namespaceName, fileNode))
      case None => None
    }
    val typeDecl: Option[nodes.TypeDecl] = typeDeclName match {
      case Some(typeName) =>
        Some(getUniqueTypeDeclaration(typeName, namespaceNode, fileNode))
      case None => None
    }
    ((namespaceNode, typeDecl) match {
      case (_, Some(typeDecl)) =>
        typeDecl.astChildren.isMethod.nameExact(name).l
      case (Some(namespace), None) =>
        cpg.method
          .astParentTypeExact("NAMESPACE_BLOCK")
          .astParentFullNameExact(namespace.fullName)
          .nameExact(name)
          .l
      case (None, None) => cpg.method.nameExact(name).l
    }) match {
      case Nil =>
        throw new MethodNotFoundException(name,
                                          typeDecl,
                                          namespaceNode,
                                          fileNode,
                                          Nil)
      case single :: Nil => single
      case multiple =>
        (start, end) match {
          case (Some(start), Some(_)) =>
            multiple
              .find(method => method.lineNumber.contains(start)) // && method.lineNumberEnd.contains(end)
              .getOrElse(
                throw new MethodNotFoundException(name,
                                                  typeDecl,
                                                  namespaceNode,
                                                  fileNode,
                                                  multiple))
          case _ =>
            throw new MethodNotFoundException(name,
                                              typeDecl,
                                              namespaceNode,
                                              fileNode,
                                              multiple)
        }

    }
  }

  def getUniqueFile(file: String)(implicit cpg: Cpg): nodes.File = {
    cpg.file.nameExact(file).l match {
      case Nil         => throw new FileNotFoundException(file, Nil)
      case file :: Nil => file
      case result      => throw new FileNotFoundException(file, result)
    }
  }

  def getUniqueNamespace(name: String, fileNode: Option[nodes.File])(
      implicit cpg: Cpg): nodes.NamespaceBlock = {
    (fileNode match {
      case Some(fileNode) =>
        fileNode.astChildren.isNamespaceBlock.nameExact(name).l
      case None => cpg.namespaceBlock.nameExact(name).l
    }) match {
      case Nil              => throw new NamespaceNotFoundException(name, fileNode, Nil)
      case namespace :: Nil => namespace
      case results =>
        throw new NamespaceNotFoundException(name, fileNode, results)
    }
  }

  def getUniqueTypeDeclaration(
      typeName: String,
      namespaceNode: Option[nodes.NamespaceBlock],
      fileNode: Option[nodes.File])(implicit cpg: Cpg): nodes.TypeDecl = {
    ((fileNode, namespaceNode) match {
      case (_, Some(namespace)) =>
        cpg.typeDecl
          .astParentTypeExact("NAMESPACE_BLOCK")
          .astParentFullNameExact(namespace.fullName)
          .nameExact(typeName)
          .l
      case (Some(file), None) =>
        file.astChildren.isNamespaceBlock
          .map { namespace =>
            cpg.typeDecl
              .astParentTypeExact("NAMESPACE_BLOCK")
              .astParentFullNameExact(namespace.fullName)
          }
          .foldLeft(Traversal[nodes.TypeDecl]())(_ ++ _)
          .astChildren
          .isTypeDecl
          .nameExact(typeName)
          .l
      case (None, None) => cpg.typeDecl.nameExact(typeName).l
    }) match {
      case Nil =>
        throw new TypeDeclNotFoundException(typeName,
                                            namespaceNode,
                                            fileNode,
                                            Nil)
      case typeDecl :: Nil => typeDecl
      case results =>
        throw new TypeDeclNotFoundException(typeName,
                                            namespaceNode,
                                            fileNode,
                                            results)
    }
  }

  def getAbsolutePath(path: String): String = {
    new File(path).toPath.toAbsolutePath.normalize().toString
  }

}
