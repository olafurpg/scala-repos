package mesosphere.marathon.state

import com.codahale.metrics.MetricRegistry
import mesosphere.marathon.MarathonSpec
import mesosphere.marathon.metrics.Metrics
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import org.mockito.Mockito._
import org.scalatest.Matchers
import scala.language.postfixOps
import PathId._

class GroupRepositoryTest extends MarathonSpec with Matchers

  test("Store canary strategy")
    val store = mock[MarathonStore[Group]]
    val group = Group("g1".toPath, Set.empty)
    val future = Future.successful(group)
    val versionedKey = s"root:${group.version}"
    val appRepo = mock[AppRepository]

    when(store.store(versionedKey, group)).thenReturn(future)
    when(store.store("root", group)).thenReturn(future)

    val metrics = new Metrics(new MetricRegistry)
    val repo = new GroupRepository(store, None, metrics)
    val res = repo.store("root", group)

    assert(group == Await.result(res, 5 seconds),
           "Should return the correct Group")
    verify(store).store(versionedKey, group)
    verify(store).store(s"root", group)

  test("group back and forth again with rolling strategy")
    val group = Group("g1".toPath, Set.empty)
    val proto = group.toProto
    val merged = Group.fromProto(proto)
    group should be(merged)
