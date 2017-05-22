/*                     __                                               *\
 **     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
 **    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
 **  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
 ** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
 **                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.utils

import scala.scalajs.runtime

object Platform

  /** Returns `true` if and only if the code is executing on a JVM.
    *  Note: Returns `false` when executing on any JS VM.
    */
  final val executingInJVM = false

  final val executingInJVMOnJDK6 = false

  final val executingInJVMOnJDK7OrLower = false

  // Members that are only accessible from testSuite/js
  // (i.e. do no link on the JVM).

  def areTypedArraysSupported: Boolean =
    runtime.Bits.areTypedArraysSupported

  def executingInRhino: Boolean = sysProp("rhino")
  def executingInNodeJS: Boolean = sysProp("nodejs")
  def executingInPhantomJS: Boolean = sysProp("phantomjs")
  def typedArrays: Boolean = sysProp("typedarray")
  def sourceMaps: Boolean = sysProp("source-maps")

  def isInFastOpt: Boolean = sysProp("fastopt-stage")
  def isInFullOpt: Boolean = sysProp("fullopt-stage")
  def isInProductionMode: Boolean = sysProp("production-mode")
  def isInDevelopmentMode: Boolean = sysProp("development-mode")

  def hasCompliantAsInstanceOfs: Boolean = sysProp("compliant-asinstanceofs")
  def hasCompliantModule: Boolean = sysProp("compliant-moduleinit")
  def hasStrictFloats: Boolean = sysProp("strict-floats")

  private def sysProp(key: String): Boolean =
    System.getProperty("scalajs." + key, "false") == "true"
