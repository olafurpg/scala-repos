package org.jetbrains.plugins.scala.extensions

import com.intellij.psi.PsiElement
import com.intellij.psi.tree.IElementType

/**
  * Pavel Fatin
  */
object ElementType
  def unapply(e: PsiElement): Option[IElementType] =
    Option(e.getNode.getElementType)
