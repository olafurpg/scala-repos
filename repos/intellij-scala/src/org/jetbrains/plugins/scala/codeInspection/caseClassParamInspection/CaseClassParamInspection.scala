package org.jetbrains.plugins.scala
package codeInspection
package caseClassParamInspection

import com.intellij.codeInspection._
import com.intellij.psi.{PsiElement, PsiElementVisitor}
import org.jetbrains.plugins.scala.lang.psi.api.ScalaElementVisitor
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScClassParameter
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScClass, ScTypeDefinition}

class CaseClassParamInspection
    extends AbstractInspection("CaseClassParam", "Case Class Parameter")
  override def actionFor(
      holder: ProblemsHolder): PartialFunction[PsiElement, Any] =
    case c: ScClass if c.isCase =>
      for
        paramClause <- c.allClauses.take(1)
        classParam @ (__ : ScClassParameter) <- paramClause.parameters
                                                   if classParam.isVal &&
                                               classParam.isCaseClassVal
      
        holder.registerProblem(
            classParam,
            ScalaBundle.message("val.on.case.class.param.redundant"),
            ProblemHighlightType.GENERIC_ERROR_OR_WARNING,
            new RemoveValQuickFix(classParam))
