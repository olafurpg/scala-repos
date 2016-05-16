// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.util

import org.slf4j.LoggerFactory

import scala.reflect.internal.util.Position
import scala.tools.nsc.reporters.Reporter

import org.ensime.api._
import org.ensime.core.PositionBackCompat

trait ReportHandler {
  def messageUser(str: String): Unit = {}
  def clearAllScalaNotes(): Unit = {}
  def reportScalaNotes(notes: List[Note]): Unit = {}
  def clearAllJavaNotes(): Unit = {}
  def reportJavaNotes(notes: List[Note]): Unit = {}
}

class PresentationReporter(handler: ReportHandler)
    extends Reporter
    with PositionBackCompat {

  val log = LoggerFactory.getLogger(classOf[PresentationReporter])
  private var enabled = true
  def enable(): Unit = { enabled = true }
  def disable(): Unit = { enabled = false }

  override def reset(): Unit = {
    super.reset()
    if (enabled) {
      handler.clearAllScalaNotes()
    }
  }

  override def info0(
      pos: Position, msg: String, severity: Severity, force: Boolean): Unit = {
    severity.count += 1
    try {
      if (severity.id == 0) {
        log.info(msg)
      } else {
        if (enabled) {
          if (pos.isDefined) {
            val source = pos.source
            val f = source.file.absolute.path
            val posColumn =
              if (pos.point == -1) {
                0
              } else {
                pos.column
              }

            val note = new Note(
                f,
                formatMessage(msg),
                NoteSeverity(severity.id),
                pos.startOrCursor,
                pos.endOrCursor,
                pos.line,
                posColumn
            )
            handler.reportScalaNotes(List(note))
          }
        }
      }
    } catch {
      case ex: UnsupportedOperationException =>
        log.warn("Unsupported operation during reporting", ex)
    }
  }

  def formatMessage(msg: String): String = {
    augmentString(msg).map {
      case '\n' | '\r' => ' '
      case c => c
    }
  }
}
