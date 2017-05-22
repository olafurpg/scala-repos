package org.jetbrains.plugins.scala
package annotator
package gutter

import com.intellij.openapi.util.IconLoader

/**
  * User: Alexander Podkhalyuzin
  * Date: 31.10.2008
  */
object GutterIcons
  val RECURSION_ICON =
    IconLoader.getIcon("/org/jetbrains/plugins/scala/gator/recursion.png")
  val TAIL_RECURSION_ICON =
    IconLoader.getIcon("/org/jetbrains/plugins/scala/gator/tail-recursion.png")

  val OVERRIDING_METHOD_ICON =
    IconLoader.getIcon("/gutter/overridingMethod.png")
  val IMPLEMENTING_METHOD_ICON =
    IconLoader.getIcon("/gutter/implementingMethod.png")

  val OVERRIDEN_METHOD_MARKER_RENDERER =
    IconLoader.getIcon("/gutter/overridenMethod.png")
  val IMPLEMENTED_METHOD_MARKER_RENDERER =
    IconLoader.getIcon("/gutter/implementedMethod.png")
  val IMPLEMENTED_INTERFACE_MARKER_RENDERER =
    IMPLEMENTED_METHOD_MARKER_RENDERER
  val SUBCLASSED_CLASS_MARKER_RENDERER = OVERRIDEN_METHOD_MARKER_RENDERER
