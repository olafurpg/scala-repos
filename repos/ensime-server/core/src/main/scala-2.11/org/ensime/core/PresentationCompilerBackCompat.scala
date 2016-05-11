// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import Predef.{any2stringadd => _}

import scala.reflect.internal.util.Position

/**
 * Simulate methods that were added in later versions of the scalac
 * API, or to generate fake methods that we can use in both versions.
 */
trait PresentationCompilerBackCompat

trait PositionBackCompat {
  implicit class RichPosition(pos: Position) {
    // annoyingly, {start, end}OrPoint is deprecated
    def startOrCursor: Int = pos.start
    def endOrCursor: Int = pos.end
  }
}
