// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.indexer.lucene

import org.apache.lucene.document.Document

trait DocumentRecovery[T] {
  def toEntity(d: Document): T
}
