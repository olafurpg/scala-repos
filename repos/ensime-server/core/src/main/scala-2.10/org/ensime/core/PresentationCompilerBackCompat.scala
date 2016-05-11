// Copyright 2010 - 2015 https://github.com/ensime/ensime-server/graphs/
// License: GPL 3.0
package org.ensime.core

import Predef.{any2stringadd => _, _}

import scala.reflect.internal.util.Position
import scala.reflect.internal.util.SourceFile

/**
 * Simulate methods that were added in later versions of the scalac
 * API, or to generate fake methods that we can use in both versions.
 */
trait PresentationCompilerBackCompat {
  this: RichPresentationCompiler =>

  implicit class RichSymbols(sym: Symbol) {
    def isLocalToBlock: Boolean = sym.isLocal

    def paramLists: List[List[Symbol]] = sym.paramss
  }
}

trait PositionBackCompat {
  implicit class RichPosition(pos: Position) {
    def withSource(src: SourceFile): Position =
      pos.withSource(src, 0)

    def withShift(shift: Int): Position =
      pos.withSource(pos.source, shift)

    // I wish we could override `start` and `end`
    def startOrCursor: Int = pos.startOrPoint
    def endOrCursor: Int = pos.endOrPoint
  }
}
