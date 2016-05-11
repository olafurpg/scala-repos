// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.util

import java.io.File
import org.ensime.api._

object FileEditHelper {

  import scala.tools.refactoring.common.{ Change, TextChange }

  def fromChange(ch: Change): FileEdit = {
    ch match {
      case ch: TextChange => TextEdit(ch.file.file, ch.from, ch.to, ch.text)
      case _ => throw new UnsupportedOperationException(ch.toString)
    }
  }

  def applyEdits(ch: List[TextEdit], source: String): String = {
    (source /: ch.sortBy(-_.to)) { (src, change) =>
      src.substring(0, change.from) + change.text + src.substring(change.to)
    }
  }

  def diffFromTextEdits(ch: List[TextEdit], source: String, originalFile: File, revisedFile: File): String = {
    val newContents = applyEdits(ch, source)
    DiffUtil.compareContents(source.lines.toSeq, newContents.lines.toSeq, originalFile, revisedFile)
  }

  //TODO: add diffFromNewFile and diffFromDeleteFile
  //def diffFromNewFile(ch: NewFile, source: String): String = ???
  //def diffFromDeleteFile(ch: DeleteFile, source: String): String = ??
}
