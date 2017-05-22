package org.jetbrains.plugins.scala
package lang
package psi
package api
package statements

import com.intellij.psi.PsiNamedElement

trait ScDeclaredElementsHolder extends ScalaPsiElement
  def declaredElements: Seq[PsiNamedElement]

  /**
    * @return array for Java compatibility [[org.jetbrains.plugins.scala.gotoclass.ScalaGoToSymbolContributor]]
    */
  def declaredElementsArray: Array[PsiNamedElement] = declaredElements.toArray
