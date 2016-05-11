// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core.javac

import akka.event.slf4j.SLF4JLogging
import com.sun.source.tree.Tree
import javax.lang.model.element.Element

import com.sun.tools.javac.tree.JCTree
import com.sun.tools.javac.tree.JCTree._

/**
 * This trait provides behavior missing from jdk 1.6, 1.7, see:
 * http://hg.openjdk.java.net/jdk8/jdk8/langtools/rev/8dd528992c15 and
 * sadly depends on unsupported implementation classes,
 * (com.sun.tools.javac.*). As a result, this may stop working on on
 * Java 9. At that point we can either:
 * 1) Take the long way around and find Elements ourselves by searching the
 *   scope/target for the selected name (see my older version of JavaDocFinding).
 * 2) Use reflection here.
 * 3) Convince Java 9 to let us import these unsafe libs.
 *
 * -aemon
 */
trait UnsafeHelpers extends SLF4JLogging {

  protected def unsafeGetElement(info: CompilationInfo, t: Tree): Option[Element] = {
    t match {
      case t: JCCompilationUnit => Some(t.packge)
      case t: JCClassDecl => Some(t.sym)
      case t: JCMethodDecl => Some(t.sym)
      case t: JCVariableDecl => Some(t.sym)
      case t: JCIdent => Some(t.sym)
      case t: JCFieldAccess => Some(t.sym)
      // TODO: Workaround for java 6
      //      case t: JCMemberReference => Some(t.sym)
      case t: JCNewClass => Some(t.constructor)
      case t: JCMethodInvocation => unsafeGetElement(info, t.meth)
      case t: JCTypeApply => unsafeGetElement(info, t.clazz)
      case t: JCTree => if (t.`type` != null) Some(t.`type`.tsym) else None
      case _ => None
    }
  }

}
