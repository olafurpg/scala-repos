// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.indexer.lucene

import java.io.StringReader
import org.apache.lucene.analysis.core.KeywordTokenizer
import org.apache.lucene.analysis.tokenattributes.CharTermAttribute
import org.ensime.util.EnsimeSpec
import DynamicSynonymFilter._
import scala.collection.mutable

class DynamicSynonymFilterSpec extends EnsimeSpec {

  val cleese = Set(
    "resting", "stunned", "deceased", "passed on",
    "no more", "ceased to be", "expired and gone to meet his maker",
    "stiff", "bereft of life", "rests in peace", "pushing up the daisies",
    "metabolic processes are history", "off the twig", "kicked the bucket",
    "shuffled off his mortal coil",
    "run down the curtain and joined the bleedin choir invisible",
    "ex-parrot"
  )
  val engine = new SynonymEngine {
    def synonyms(term: String) =
      if (term != "dead") Set.empty
      else cleese
  }

  private def applyEngineToTerm(term: String, engine: SynonymEngine): List[String] = {
    val reader = new StringReader(term)
    val source = new KeywordTokenizer(reader)
    val filter = new DynamicSynonymFilter(source, engine)

    val words: mutable.ListBuffer[String] = mutable.ListBuffer()
    filter.reset()
    while (filter.incrementToken()) {
      words += source.getAttribute(classOf[CharTermAttribute]).toString
    }
    filter.close()
    words.toList.sorted
  }

  "DynamicSynonymFilter" should "not add synonyms where there are none" in {
    val term = "Norwegian Blue"
    applyEngineToTerm(term, engine) should ===(List(term))
  }

  it should "report known synonyms" in {
    val term = "dead"
    applyEngineToTerm(term, engine) should ===((cleese + term).toList.sorted)
  }
}
