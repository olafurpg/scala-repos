package org.jetbrains.plugins.scala
package lang
package psi
package impl
package statements

import com.intellij.lang.ASTNode
import com.intellij.openapi.progress.ProgressManager
import com.intellij.psi._
import com.intellij.psi.scope.PsiScopeProcessor
import com.intellij.psi.stubs.StubElement
import com.intellij.psi.tree.IElementType
import org.jetbrains.plugins.scala.extensions.PsiElementExt
import org.jetbrains.plugins.scala.lang.lexer._
import org.jetbrains.plugins.scala.lang.parser.ScalaElementTypes
import org.jetbrains.plugins.scala.lang.psi.api.statements._
import org.jetbrains.plugins.scala.lang.psi.api.statements.params._
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScTypeParametersOwner
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScMember
import org.jetbrains.plugins.scala.lang.psi.stubs.ScFunctionStub

/**
  * @author ilyas
  */
abstract class ScFunctionImpl protected (stub: StubElement[ScFunction],
                                         nodeType: IElementType,
                                         node: ASTNode)
    extends ScalaStubBasedElementImpl(stub, nodeType, node)
    with ScMember
    with ScFunction
    with ScTypeParametersOwner {
  override def isStable = false

  def nameId: PsiElement = {
    val n = getNode.findChildByType(ScalaTokenTypes.tIDENTIFIER) match {
      case null => getNode.findChildByType(ScalaTokenTypes.kTHIS)
      case notNull => notNull
    }
    if (n == null) {
      return ScalaPsiElementFactory
        .createIdentifier(getStub.asInstanceOf[ScFunctionStub].getName,
                          getManager)
        .getPsi
    }
    n.getPsi
  }

  def paramClauses: ScParameters = {
    getStubOrPsiChild(ScalaElementTypes.PARAM_CLAUSES)
  }

  override def processDeclarations(processor: PsiScopeProcessor,
                                   state: ResolveState,
                                   lastParent: PsiElement,
                                   place: PsiElement): Boolean = {
    // process function's process type parameters
    if (!super[ScTypeParametersOwner].processDeclarations(processor,
                                                          state,
                                                          lastParent,
                                                          place))
      return false

    lazy val parameterIncludingSynthetic: Seq[ScParameter] =
      effectiveParameterClauses.flatMap(_.effectiveParameters)
    if (getStub == null) {
      returnTypeElement match {
        case Some(x)
            if lastParent != null &&
              x.startOffsetInParent == lastParent.startOffsetInParent =>
          for (p <- parameterIncludingSynthetic) {
            ProgressManager.checkCanceled()
            if (!processor.execute(p, state)) return false
          }
        case _ =>
      }
    } else {
      if (lastParent != null &&
          lastParent.getContext != lastParent.getParent) {
        for (p <- parameterIncludingSynthetic) {
          ProgressManager.checkCanceled()
          if (!processor.execute(p, state)) return false
        }
      }
    }
    true
  }
}
