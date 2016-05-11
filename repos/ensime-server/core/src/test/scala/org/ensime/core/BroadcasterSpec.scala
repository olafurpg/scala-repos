// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import akka.testkit._
import scala.concurrent.duration._
import org.ensime.fixture.SharedTestKitFixture
import org.ensime.util.EnsimeSpec

class BroadcasterSpec extends EnsimeSpec with SharedTestKitFixture {
  import Broadcaster._

  val ping = "ping"

  "Broadcaster" should "send messages to subscribers" in withTestKit { fix =>
    import fix._
    val broadcaster = TestActorRef[Broadcaster]
    val sub1 = TestProbe()
    val sub2 = TestProbe()

    sub1.send(broadcaster, Register)
    sub2.send(broadcaster, Register)

    broadcaster ! ping

    sub1.expectMsg(ping)
    sub1.lastSender shouldBe self
    sub2.expectMsg(ping)
    sub2.lastSender shouldBe self
  }

  it should "not send messages after unregister" in withTestKit { fix =>
    import fix._
    val broadcaster = TestActorRef[Broadcaster]
    val sub1 = TestProbe()
    val sub2 = TestProbe()

    sub1.send(broadcaster, Register)
    sub1.send(broadcaster, Unregister)

    broadcaster ! ping

    sub1.expectNoMsg(3 seconds)
  }

  it should "send persistent messages on registration" in withTestKit { fix =>
    import fix._
    val broadcaster = TestActorRef[Broadcaster]
    val sub1 = TestProbe()

    broadcaster ! Persist(ping)

    sub1.expectNoMsg(3 seconds)
    sub1.send(broadcaster, Register)
    sub1.expectMsg(ping)
    sub1.lastSender shouldBe self

    // and on reregistration
    sub1.send(broadcaster, Register)
    sub1.expectMsg(ping)
    sub1.lastSender shouldBe self
  }

}
