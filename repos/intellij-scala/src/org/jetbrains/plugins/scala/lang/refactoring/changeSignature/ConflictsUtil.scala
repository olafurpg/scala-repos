package org.jetbrains.plugins.scala
package lang.refactoring.changeSignature

import com.intellij.psi.PsiElement
import com.intellij.refactoring.changeSignature.{ChangeInfo, JavaChangeInfo, OverriderUsageInfo}
import com.intellij.util.containers.MultiMap
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil
import org.jetbrains.plugins.scala.lang.psi.api.base.ScPrimaryConstructor
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.ScBindingPattern
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScClassParameter
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScPatternDefinition, ScVariableDefinition}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScMember
import org.jetbrains.plugins.scala.lang.psi.types.ValType
import org.jetbrains.plugins.scala.lang.refactoring.changeSignature.changeInfo.ScalaChangeInfo

/**
  * Nikolay.Tropin
  * 2014-08-13
  */
private[changeSignature] object ConflictsUtil

  type ConflictsMap = MultiMap[PsiElement, String]

  def addJavaOverriderConflicts(info: OverriderUsageInfo,
                                change: ChangeInfo,
                                map: ConflictsMap): Unit =
    change match
      case sc: ScalaChangeInfo
          if sc.newParameters.exists(
              p => p.isByName && p.scType.isInstanceOf[ValType]) =>
        val message =
          s"This method has java overriders, by-name parameters of value classes cannot be used."
        map.putValue(info.getElement, message)
      case _ =>

  def addBindingPatternConflicts(bp: ScBindingPattern,
                                 change: ChangeInfo,
                                 result: ConflictsMap): Unit =

    if (change.getNewParameters.nonEmpty)
      val (member: ScMember, kind, isSimple) = bp match
        case ScalaPsiUtil.inNameContext(pd: ScPatternDefinition) =>
          (pd, "pattern definition", pd.isSimple)
        case ScalaPsiUtil.inNameContext(vd: ScVariableDefinition) =>
          (vd, "variable definition", vd.isSimple)
        case _ => return

      if (!isSimple)
        val className = member.containingClass.qualifiedName
        val message =
          s"Method is overriden in a composite $kind in $className. " +
          "Converting it to function definition is not supported."

        result.putValue(bp, message)

  def addClassParameterConflicts(
      cp: ScClassParameter, change: ChangeInfo, result: ConflictsMap): Unit =
    if (change.getNewParameters.nonEmpty)
      val className = cp.containingClass.qualifiedName
      val message =
        s"Method is overriden by class parameter of $className. " +
        "Converting it to a function definition is not supported."
      result.putValue(cp, message)

  def addUnapplyUsagesConflicts(
      p: PatternUsageInfo, change: ChangeInfo, result: ConflictsMap): Unit =
    change match
      case jc: JavaChangeInfo
          if jc.isParameterSetOrOrderChanged || jc.isParameterTypesChanged =>
        jc.getMethod match
          case ScPrimaryConstructor.ofClass(clazz) if clazz.isCase =>
            val message =
              "Updating of usages of generated `unapply` methods is not supported"
            result.putValue(p.pattern, message)
          case _ =>
      case _ =>
