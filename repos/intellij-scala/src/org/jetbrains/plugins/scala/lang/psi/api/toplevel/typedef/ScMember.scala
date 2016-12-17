package org.jetbrains.plugins.scala
package lang
package psi
package api
package toplevel
package typedef

import com.intellij.psi._
import com.intellij.psi.impl.source.PsiFileImpl
import com.intellij.psi.search.{LocalSearchScope, PackageScope, SearchScope}
import com.intellij.psi.stubs.StubElement
import com.intellij.psi.util._
import org.jetbrains.plugins.scala.lang.psi.api.base.{
  ScAccessModifier,
  ScPrimaryConstructor
}
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScBlock
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScClassParameter
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.templates.{
  ScExtendsBlock,
  ScTemplateBody
}
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaFileImpl
import org.jetbrains.plugins.scala.lang.psi.stubs.ScMemberOrLocal

import scala.collection.mutable.ArrayBuffer

/**
  * @author Alexander Podkhalyuzin
  * Date: 04.05.2008
  */
trait ScMember
    extends ScalaPsiElement
    with ScModifierListOwner
    with PsiMember {
  def getContainingClass: PsiClass = containingClass

  def isPrivate: Boolean = hasModifierPropertyScala("private")

  def isProtected: Boolean = hasModifierPropertyScala("protected")

  def isPublic: Boolean = !isPrivate && !isProtected

  def isInstance: Boolean = !isLocal

  /**
    * getContainingClassStrict(bar) == null in
    *
    * `object a { def foo { def bar = 0 }}`
    */
  def containingClass: ScTemplateDefinition = {
    val stub: StubElement[_ <: PsiElement] = this match {
      case file: PsiFileImpl => file.getStub
      case st: ScalaStubBasedElementImpl[_] => st.getStub
      case _ => null
    }
    stub match {
      case m: ScMemberOrLocal if m.isLocal => return null
      case _ =>
    }
    val context = getContext
    (getContainingClassLoose, this) match {
      case (null, _) => null
      case (found, fun: ScFunction)
          if fun.syntheticContainingClass.isDefined =>
        fun.syntheticContainingClass.get
      case (found, fun: ScFunction) if fun.isSynthetic => found
      case (found, td: ScTypeDefinition)
          if td.syntheticContainingClass.isDefined =>
        td.syntheticContainingClass.get
      case (found, td: ScTypeDefinition) if td.isSynthetic => found
      case (found, _: ScClassParameter | _: ScPrimaryConstructor) => found
      case (found, _)
          if context == found.extendsBlock ||
            found.extendsBlock.templateBody.contains(context) ||
            found.extendsBlock.earlyDefinitions.contains(context) =>
        found
      case (found, _) => null // See SCL-3178
    }
  }

  def getContainingClassLoose: ScTemplateDefinition = {
    val stub: StubElement[_ <: PsiElement] = this match {
      case file: PsiFileImpl => file.getStub
      case st: ScalaStubBasedElementImpl[_] => st.getStub
      case _ => null
    }
    if (stub != null) {
      stub.getParentStubOfType(classOf[ScTemplateDefinition])
    } else {
      child match {
        // TODO is all of this mess still necessary?!
        case c: ScClass if c.isCase =>
          this match {
            case fun: ScFunction
                if fun.isSyntheticApply || fun.isSyntheticUnapply ||
                  fun.isSyntheticUnapplySeq =>
              //this is special case for synthetic apply and unapply methods
              ScalaPsiUtil.getCompanionModule(c) match {
                case Some(td) => return td
                case _ =>
              }
            case _ =>
          }
        case _ =>
      }
      PsiTreeUtil.getContextOfType(this, true, classOf[ScTemplateDefinition])
    }
  }

  def isLocal: Boolean = {
    val stub: StubElement[_ <: PsiElement] = this match {
      case file: PsiFileImpl => file.getStub
      case st: ScalaStubBasedElementImpl[_] => st.getStub
      case _ => null
    }
    stub match {
      case memberOrLocal: ScMemberOrLocal =>
        return memberOrLocal.isLocal
      case _ =>
    }
    containingClass == null
  }

  override def hasModifierProperty(name: String) = {
    name match {
      case PsiModifier.PUBLIC =>
        !hasModifierProperty("private") && !hasModifierProperty("protected")
      case PsiModifier.STATIC => containingClass.isInstanceOf[ScObject]
      case PsiModifier.PRIVATE =>
        getModifierList.accessModifier.exists(
          _.access == ScAccessModifier.Type.THIS_PRIVATE)
      case PsiModifier.PROTECTED =>
        getModifierList.accessModifier.exists(
          _.access == ScAccessModifier.Type.THIS_PROTECTED)
      case _ => super.hasModifierProperty(name)
    }
  }

  protected def isSimilarMemberForNavigation(m: ScMember, isStrict: Boolean) =
    false

  override def getNavigationElement: PsiElement = getContainingFile match {
    case s: ScalaFileImpl if s.isCompiled => getSourceMirrorMember
    case _ => this
  }

  private def getSourceMirrorMember: ScMember = getParent match {
    case tdb: ScTemplateBody =>
      tdb.getParent match {
        case eb: ScExtendsBlock =>
          eb.getParent match {
            case td: ScTypeDefinition =>
              td.getNavigationElement match {
                case c: ScTypeDefinition =>
                  val membersIterator = c.members.iterator
                  val buf: ArrayBuffer[ScMember] = new ArrayBuffer[ScMember]
                  while (membersIterator.hasNext) {
                    val member = membersIterator.next()
                    if (isSimilarMemberForNavigation(member, isStrict = false))
                      buf += member
                  }
                  if (buf.isEmpty) this
                  else if (buf.length == 1) buf(0)
                  else {
                    val filter = buf.filter(
                      isSimilarMemberForNavigation(_, isStrict = true))
                    if (filter.isEmpty) buf(0)
                    else filter(0)
                  }
                case _ => this
              }
            case _ => this
          }
        case _ => this
      }
    case c: ScTypeDefinition if this.isInstanceOf[ScPrimaryConstructor] =>
      //primary constructor
      c.getNavigationElement match {
        case td: ScClass =>
          td.constructor match {
            case Some(constr) => constr
            case _ => this
          }
        case _ => this
      }
    case _ => this
  }

  abstract override def getUseScope: SearchScope = {
    val accessModifier = Option(getModifierList).flatMap(_.accessModifier)

    def fromContainingBlockOrMember(): Option[SearchScope] = {
      val blockOrMember = PsiTreeUtil.getContextOfType(this,
                                                       true,
                                                       classOf[ScBlock],
                                                       classOf[ScMember])
      blockOrMember match {
        case null => None
        case block: ScBlock => Some(new LocalSearchScope(block))
        case member: ScMember => Some(member.getUseScope)
      }
    }

    def fromQualifiedPrivate(): Option[SearchScope] = {
      accessModifier
        .filter(am => am.isPrivate && am.getReference != null)
        .map(_.scope) collect {
        case p: PsiPackage => new PackageScope(p, true, true)
        case td: ScTypeDefinition => ScalaPsiUtil.withCompanionSearchScope(td)
      }
    }

    val fromModifierOrContext = this match {
      case _ if accessModifier.exists(mod => mod.isPrivate && mod.isThis) =>
        Option(containingClass)
          .orElse(containingFile)
          .map(new LocalSearchScope(_))
      case _ if accessModifier.exists(_.isUnqualifiedPrivateOrThis) =>
        containingClass match {
          case null => containingFile.map(new LocalSearchScope(_))
          case c => Some(ScalaPsiUtil.withCompanionSearchScope(c))
        }
      case cp: ScClassParameter =>
        Option(cp.containingClass)
          .map(_.getUseScope)
          .orElse(Option(super.getUseScope))
      case fun: ScFunction if fun.isSynthetic =>
        fun.getSyntheticNavigationElement.map(_.getUseScope)
      case _ =>
        fromQualifiedPrivate().orElse(fromContainingBlockOrMember())
    }
    ScalaPsiUtil.intersectScopes(super.getUseScope, fromModifierOrContext)
  }
}
