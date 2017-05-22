/*                     __                                               *\
 **     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
 **    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
 **  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
 ** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
 **                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.niobuffer

import org.junit.{Assume, BeforeClass}

import org.scalajs.testsuite.utils.Platform

trait SupportsTypedArrays
  @BeforeClass def assumeThatContextSupportsTypedByteArrays(): Unit =
    Assume.assumeTrue(Platform.areTypedArraysSupported)
