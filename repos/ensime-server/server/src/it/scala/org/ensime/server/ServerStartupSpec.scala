// Copyright: 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.server

import akka.actor.Props
import java.net.{ InetSocketAddress, ServerSocket, Socket }
import org.ensime.fixture.{ EnsimeConfigFixture, IsolatedEnsimeConfigFixture, IsolatedTestKitFixture }
import org.ensime.util.EnsimeSpec
import scala.concurrent.duration._
import org.ensime.util.file._
import scala.util.{ Properties, Try }

class ServerStartupSpec extends EnsimeSpec
    with IsolatedEnsimeConfigFixture
    with IsolatedTestKitFixture {

  val original = EnsimeConfigFixture.EmptyTestProject

  Properties.setProp("ensime.server.test", "true")

  "Server" should "start up and bind to random ports" in {
    withEnsimeConfig { implicit config =>
      withTestKit { implicit tk =>
        import tk._

        val protocol = new SwankProtocol
        system.actorOf(Props(new ServerActor(config, protocol)), "ensime-main")

        eventually(timeout(30 seconds), interval(1 second)) {
          PortUtil.port(config.cacheDir, "http").isDefined
          PortUtil.port(config.cacheDir, "port").isDefined
        }
      }
    }
  }

  it should "start up and bind to preferred ports" in {
    withEnsimeConfig { implicit config =>
      withTestKit { implicit tk =>
        import tk._

        // this can fail randomly. No general solution.
        val preferredHttp = 10001
        val preferredTcp = 10002

        (config.cacheDir / "http").writeString(preferredHttp.toString)
        (config.cacheDir / "port").writeString(preferredTcp.toString)

        val protocol = new SwankProtocol
        system.actorOf(Props(new ServerActor(config, protocol)), "ensime-main")

        eventually(timeout(30 seconds), interval(1 second)) {
          val http = new Socket
          val tcp = new Socket

          try {
            http.connect(new InetSocketAddress("127.0.0.1", preferredHttp))
            tcp.connect(new InetSocketAddress("127.0.0.1", preferredTcp))

            http.isConnected() && tcp.isConnected()
          } finally {
            Try(http.close())
            Try(tcp.close())
          }
        }
      }
    }
  }

  it should "shutdown if preferred TCP port is not available" in {
    withEnsimeConfig { implicit config =>
      withTestKit { implicit tk =>
        import tk._

        val preferredTcp = 10004
        (config.cacheDir / "port").writeString(preferredTcp.toString)
        val tcpHog = new ServerSocket().bind(new InetSocketAddress("127.0.0.1", preferredTcp))

        val protocol = new SwankProtocol
        system.actorOf(Props(new ServerActor(config, protocol)), "ensime-main")

        eventually(timeout(30 seconds), interval(1 second)) {
          system.isTerminated
        }
      }
    }
  }

  it should "shutdown if preferred HTTP port is not available" in {
    withEnsimeConfig { implicit config =>
      withTestKit { implicit tk =>
        import tk._

        val preferredHttp = 10003
        (config.cacheDir / "http").writeString(preferredHttp.toString)

        val httpHog = new ServerSocket().bind(new InetSocketAddress("127.0.0.1", preferredHttp))

        val protocol = new SwankProtocol
        system.actorOf(Props(new ServerActor(config, protocol)), "ensime-main")

        eventually(timeout(30 seconds), interval(1 second)) {
          system.isTerminated
        }
      }
    }
  }

}
