// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core.javac

import akka.event.slf4j.SLF4JLogging
import com.sun.source.tree.{ Tree, IdentifierTree }
import com.sun.source.util.TreePath
import javax.lang.model.`type`.{ DeclaredType, PrimitiveType, TypeKind, TypeMirror }
import javax.lang.model.element.ElementKind
import javax.lang.model.element.{ Element, TypeElement }
import org.ensime.core.{ DocFqn, DocSig }

case class JavaFqn(pack: Option[String], typename: Option[String], fieldOrMethod: Option[String]) {
  def toDocSig = DocSig(DocFqn(pack.getOrElse(""), typename.getOrElse("")), fieldOrMethod)
  def toFqnString = Array(pack, typename, fieldOrMethod).flatten.mkString(".")
  def toQueryString = Array(pack, typename.map(_.replace(".", "$")), fieldOrMethod).flatten.mkString(".")
}

object JavaFqn {
  def apply(pack: String, tpe: String, fieldOrMethod: Option[String]): JavaFqn = {
    JavaFqn(
      if (pack.isEmpty) None else Some(pack),
      if (tpe.isEmpty) None else Some(tpe),
      fieldOrMethod
    )
  }
}

trait Helpers extends UnsafeHelpers with SLF4JLogging {

  def typeMirror(info: CompilationInfo, t: Tree): Option[TypeMirror] = {
    Option(info.getTrees().getTypeMirror(info.getTrees().getPath(info.getCompilationUnit(), t)))
  }

  def typeElement(info: CompilationInfo, t: Tree): Option[Element] = {
    typeMirror(info, t).map(info.getTypes().asElement)
  }

  def element(info: CompilationInfo, path: TreePath): Option[Element] = {
    Option(info.getTrees.getElement(path)).orElse(unsafeGetElement(info, path.getLeaf)).orElse {
      Option(info.getTrees().getTypeMirror(path)).flatMap { t => Option(info.getTypes.asElement(t)) }
    }
  }

  private def parseFqnAsClass(s: String): Option[JavaFqn] = {
    val (front, back) = s.split("\\.").partition { s => s.forall(Character.isLowerCase) }
    Some(JavaFqn(front.mkString("."), back.mkString("."), None))
  }

  def fqn(info: CompilationInfo, el: Element): Option[JavaFqn] = {
    val kind = el.getKind
    if (kind == ElementKind.LOCAL_VARIABLE || kind == ElementKind.PARAMETER) {
      Some(JavaFqn(None, None, Some(el.getSimpleName.toString)))
    } else if (kind == ElementKind.CONSTRUCTOR || kind == ElementKind.ENUM_CONSTANT ||
      kind == ElementKind.METHOD || kind == ElementKind.FIELD) {
      Option(el.getEnclosingElement).flatMap(fqn(info, _)).map(_.copy(fieldOrMethod = Some(el.toString)))
    } else {
      parseFqnAsClass(el.toString)
    }
  }

  def fqn(info: CompilationInfo, p: TreePath): Option[JavaFqn] = {
    element(info, p).flatMap(fqn(info, _)).orElse({
      p.getLeaf match {
        case t: IdentifierTree => Some(JavaFqn(None, None, Some(t.getName.toString)))
        case t => None
      }
    }).orElse(fqn(info, info.getTrees().getTypeMirror(p)))
  }

  def fqn(info: CompilationInfo, t: Tree): Option[JavaFqn] = {
    Option(info.getTrees().getPath(info.getCompilationUnit(), t)).flatMap { p => fqn(info, p) }
  }

  def fqn(info: CompilationInfo, tm: TypeMirror): Option[JavaFqn] = {
    // "Using instanceof is not necessarily a reliable idiom for
    // determining the effective class of an object in this modeling
    // hierarchy since an implementation may choose to have a single
    // object implement multiple TypeMirror subinterfaces." --
    // TypeMirror docs
    tm match {
      case tm: DeclaredType if tm.getKind == TypeKind.DECLARED => {
        tm.asElement match {
          case te: TypeElement => parseFqnAsClass(te.getQualifiedName.toString)
          case _ => {
            None
          }
        }
      }
      case tm: PrimitiveType if tm.getKind.isPrimitive => Some(JavaFqn(None, Some(tm.toString), None))
      case _ => None
    }
  }

}
