// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.fixture

import akka.actor.ActorSystem
import org.ensime.api._
import org.ensime.vfs._
import org.ensime.indexer.SearchService
import scala.concurrent._
import scala.concurrent.duration._

trait IsolatedSearchServiceFixture extends IsolatedSourceResolverFixture {

  def withSearchService(testCode: (EnsimeConfig, SearchService) => Any)(implicit actorSystem: ActorSystem, vfs: EnsimeVFS): Any = withSourceResolver { (config, resolver) =>
    val searchService = new SearchService(config, resolver)
    try {
      testCode(config, searchService)
    } finally {
      Await.ready(searchService.shutdown(), Duration.Inf)
      actorSystem.shutdown()
      actorSystem.awaitTermination(10.seconds)
    }
  }
}

trait SharedSearchServiceFixture
    extends SharedEnsimeVFSFixture
    with SharedSourceResolverFixture {
  this: SharedTestKitFixture =>

  private[fixture] var _search: SearchService = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    implicit val system = _testkit.system
    _search = new SearchService(_config, _resolver)
  }

  override def afterAll(): Unit = {
    Await.ready(_search.shutdown(), Duration.Inf)
    super.afterAll()
  }

  def withSearchService(
    testCode: (EnsimeConfig, SearchService) => Any
  ): Unit = testCode(_config, _search)

  def withSearchService(testCode: SearchService => Any): Unit = testCode(_search)
}
