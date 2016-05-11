// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.indexer.lucene

import org.apache.lucene.document.Field._
import org.apache.lucene.document._
import org.ensime.util.EnsimeSpec

class LuceneSerializationSpec extends EnsimeSpec {

  def thereAndBackAgain[T](t: T)(implicit p: DocumentProvider[T], r: DocumentRecovery[T]): Unit = {
    val doc = p.toDocument(t)
    val back = r.toEntity(doc)
    t should ===(back)
  }

  case class SimpleThing(id: String, b: String) extends Entity
  implicit object SimpleThingS extends EntityS[SimpleThing](classOf[SimpleThing]) {
    def addFields(doc: Document, t: SimpleThing): Unit =
      doc.add(new TextField("b", t.b, Store.YES))
    def toEntity(doc: Document): SimpleThing =
      SimpleThing(doc.get("ID"), doc.get("b"))
  }

  "Lucene Entity Serialisation" should "serialise and deserialise a simple type" in {
    val t = SimpleThing("hello", "world")
    thereAndBackAgain(t)
  }

}
