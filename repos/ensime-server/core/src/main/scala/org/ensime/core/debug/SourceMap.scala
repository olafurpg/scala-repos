// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core.debug

import java.io.File

import com.sun.jdi.{ AbsentInformationException, Location }
import org.ensime.api.{ EnsimeConfig, LineSourcePosition }
import org.slf4j.LoggerFactory

import scala.collection.mutable

import org.ensime.config._
import org.ensime.util.file._

class SourceMap(config: EnsimeConfig) {
  val log = LoggerFactory.getLogger("SourceMap")

  private val sourceMap = mutable.HashMap[String, mutable.HashSet[File]]()

  def locToPos(loc: Location): Option[LineSourcePosition] = {
    try {
      (for (set <- sourceMap.get(loc.sourceName())) yield {
        if (set.size > 1) {
          log.warn(s"Warning, ambiguous source name: ${loc.sourceName()}")
        }
        set.headOption.map(f => LineSourcePosition(f, loc.lineNumber))
      }).getOrElse(None)
    } catch {
      case e: AbsentInformationException => None
    }
  }

  def rebuildSourceMap(): Unit = {
    sourceMap.clear()
    for (f <- config.scalaSourceFiles) {
      val set = sourceMap.getOrElse(f.getName, mutable.HashSet())
      set.add(f.canon)
      sourceMap(f.getName) = set
    }
  }

  // built on creation right now.
  rebuildSourceMap()
}
