/*                     __                                               *\
 **     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
 **    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
 **  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
 ** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
 **                          |/____/                                     **
\*                                                                      */

package org.scalajs.jsenv

/** Trait representing a JS console */
trait JSConsole
  def log(msg: Any): Unit
