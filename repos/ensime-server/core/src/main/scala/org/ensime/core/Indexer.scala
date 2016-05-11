// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import akka.actor._
import akka.event.LoggingReceive

import org.ensime.api._

import org.ensime.indexer.DatabaseService.FqnSymbol
import org.ensime.vfs._
import org.ensime.indexer.SearchService
import org.ensime.model._

// only used for queries by other components
case class TypeCompletionsReq(prefix: String, maxResults: Int)

class Indexer(
    index: SearchService,
    implicit val config: EnsimeConfig,
    implicit val vfs: EnsimeVFS
) extends Actor with ActorLogging {

  private def typeResult(hit: FqnSymbol) = TypeSearchResult(
    hit.fqn, hit.fqn.split("\\.").last, hit.declAs,
    LineSourcePositionHelper.fromFqnSymbol(hit)(config, vfs)
  )

  def oldSearchTypes(query: String, max: Int) =
    index.searchClasses(query, max).filterNot {
      name => name.fqn.endsWith("$") || name.fqn.endsWith("$class")
    }.map(typeResult)

  def oldSearchSymbols(terms: List[String], max: Int) =
    index.searchClassesMethods(terms, max).flatMap {
      case hit if hit.declAs == DeclaredAs.Class => Some(typeResult(hit))
      case hit if hit.declAs == DeclaredAs.Method => Some(MethodSearchResult(
        hit.fqn, hit.fqn.split("\\.").last, hit.declAs,
        LineSourcePositionHelper.fromFqnSymbol(hit)(config, vfs),
        hit.fqn.split("\\.").init.mkString(".")
      ))
      case _ => None // were never supported
    }

  override def receive = LoggingReceive {
    case ImportSuggestionsReq(file, point, names, maxResults) =>
      val suggestions = names.map(oldSearchTypes(_, maxResults))
      sender ! ImportSuggestions(suggestions)

    case PublicSymbolSearchReq(keywords, maxResults) =>
      val suggestions = oldSearchSymbols(keywords, maxResults)
      sender ! SymbolSearchResults(suggestions)

    case TypeCompletionsReq(query: String, maxResults: Int) =>
      sender ! SymbolSearchResults(oldSearchTypes(query, maxResults))

  }
}
object Indexer {
  def apply(index: SearchService)(implicit config: EnsimeConfig, vfs: EnsimeVFS): Props = Props(classOf[Indexer], index, config, vfs)
}
