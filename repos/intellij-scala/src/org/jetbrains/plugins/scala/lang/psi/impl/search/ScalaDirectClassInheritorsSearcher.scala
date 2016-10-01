package org.jetbrains.plugins.scala
package lang
package psi
package impl
package search

import com.intellij.openapi.progress.ProgressManager
import com.intellij.openapi.util.Comparing
import com.intellij.openapi.util.text.StringUtil
import com.intellij.psi.PsiClass
import com.intellij.psi.search.searches.DirectClassInheritorsSearch
import com.intellij.psi.search.{GlobalSearchScope, LocalSearchScope}
import com.intellij.psi.util.PsiUtil
import com.intellij.util.{Processor, QueryExecutor}
import org.jetbrains.plugins.scala.extensions.{PsiElementExt, inReadAction}
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScNewTemplateDefinition
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{
  ScObject,
  ScTemplateDefinition,
  ScTypeDefinition
}
import org.jetbrains.plugins.scala.lang.psi.stubs.util.ScalaStubsUtil

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * User: Alexander Podkhalyuzin
  * Date: 24.10.2008
  */
class ScalaDirectClassInheritorsSearcher
    extends QueryExecutor[PsiClass,
                          DirectClassInheritorsSearch.SearchParameters] {
  def execute(queryParameters: DirectClassInheritorsSearch.SearchParameters,
              consumer: Processor[PsiClass]): Boolean = {
    val clazz = queryParameters.getClassToProcess

    val scope = inReadAction {
      val useScope = clazz.getUseScope match {
        case _: LocalSearchScope =>
          clazz.containingScalaFile match {
            case Some(f) if f.getVirtualFile != null =>
              clazz.containingScalaFile.map(GlobalSearchScope.fileScope)
            case Some(f) => Some(GlobalSearchScope.allScope(f.getProject))
            case None => None
          }
        case global: GlobalSearchScope => Some(global)
        case _ => None
      }
      ScalaPsiUtil.intersectScopes(queryParameters.getScope, useScope) match {
        case x: GlobalSearchScope => x
        case _ => return true
      }
    }

    val anonymousClasses = new ArrayBuffer[PsiClass]()
    val map = new mutable.HashMap[String, ArrayBuffer[PsiClass]]()
    def add(clazz: PsiClass): Unit = {
      val id = inReadAction {
        clazz match {
          case o: ScObject => s"object:${o.qualifiedName}"
          case c: ScTypeDefinition => s"class:${c.qualifiedName}"
          case n: ScNewTemplateDefinition =>
            anonymousClasses += n
            return
          case _ =>
            val qualName = clazz.getQualifiedName
            if (qualName == null) {
              anonymousClasses += clazz
              return
            } else qualName
        }
      }
      val buffer = map.getOrElseUpdate(id, new ArrayBuffer[PsiClass]())
      buffer += clazz
    }
    val candidates: Seq[ScTemplateDefinition] = inReadAction {
      if (!clazz.isValid) return true

      ScalaStubsUtil.getClassInheritors(clazz, scope)
    }

    for {
      candidate <- candidates if candidate.showAsInheritor
    } {
      ProgressManager.checkCanceled()
      if (inReadAction { candidate.isInheritor(clazz, deep = false) })
        add(candidate)
    }

    if (map.nonEmpty) {
      def getJarFile(clazz: PsiClass) = inReadAction {
        PsiUtil.getJarFile(clazz)
      }

      val clazzJar = getJarFile(clazz)
      for ((_, sameNameInheritors) <- map) {
        ProgressManager.checkCanceled()
        sameNameInheritors.find { inheritor =>
          ProgressManager.checkCanceled()
          Comparing.equal(getJarFile(inheritor), clazzJar)
        } match {
          case Some(inheritor) =>
            if (!consumer.process(inheritor)) return false
          case _ if clazzJar == null => //this is possible during completion
            for (inheritor <- sameNameInheritors) {
              if (!consumer.process(inheritor)) return false
            }
          case _ =>
            val closestClass = sameNameInheritors.maxBy { inheritor =>
              val jarFile = getJarFile(inheritor)
              if (jarFile == null) 0
              else
                StringUtil.commonPrefixLength(jarFile.getCanonicalPath,
                                              clazzJar.getCanonicalPath)
            }
            if (!consumer.process(closestClass)) return false
        }
      }
    }

    if (anonymousClasses.nonEmpty && queryParameters.includeAnonymous()) {
      for (clazz <- anonymousClasses) {
        if (!consumer.process(clazz)) return false
      }
    }

    true
  }
}
