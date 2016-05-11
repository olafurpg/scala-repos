// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.fixture

import akka.actor._
import akka.testkit._
import org.ensime.api._
import org.ensime.core._
import org.ensime.vfs._
import org.ensime.indexer.SearchService
import org.scalatest._

trait AnalyzerFixture {
  def withAnalyzer(testCode: (EnsimeConfig, TestActorRef[Analyzer]) => Any): Any
}

object AnalyzerFixture {
  private[fixture] def create(search: SearchService)(implicit system: ActorSystem, config: EnsimeConfig, vfs: EnsimeVFS): TestActorRef[Analyzer] = {
    val indexer = TestProbe()
    val projectActor = TestProbe()
    TestActorRef(Analyzer(projectActor.ref, indexer.ref, search))
  }
}

trait IsolatedAnalyzerFixture
    extends AnalyzerFixture
    with IsolatedEnsimeVFSFixture
    with IsolatedSearchServiceFixture
    with IsolatedTestKitFixture {

  override def withAnalyzer(testCode: (EnsimeConfig, TestActorRef[Analyzer]) => Any): Any = {
    withVFS { implicit vfs =>
      withTestKit { testkit =>
        import testkit._
        withSearchService { (config, search) =>
          implicit val c = config
          testCode(config, AnalyzerFixture.create(search))
        }
      }
    }
  }

}

trait SharedAnalyzerFixture
    extends AnalyzerFixture
    with SharedTestKitFixture
    with SharedSearchServiceFixture
    with BeforeAndAfterAll {

  private[fixture] var analyzer: TestActorRef[Analyzer] = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    implicit val sys = _testkit.system
    implicit val config = _config
    analyzer = AnalyzerFixture.create(_search)
  }

  override def withAnalyzer(testCode: (EnsimeConfig, TestActorRef[Analyzer]) => Any): Any = testCode(_config, analyzer)
}
