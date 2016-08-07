package org.jetbrains.plugins.scala.lang.completion.postfix.templates

import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.lang.completion.postfix.templates.selector.SelectorType._
import org.jetbrains.plugins.scala.lang.completion.postfix.templates.selector.{
  SelectorConditions, AncestorSelector
}

/**
  * @author Roman.Shein
  *         Date: 24.12.2015
  */
class ScalaListPostfixTemplate
    extends ScalaStringBasedPostfixTemplate(
        "List",
        "List(expr)",
        new AncestorSelector(SelectorConditions.ANY_EXPR, All)) {
  override def getTemplateString(element: PsiElement): String = "List($expr$)"
}
