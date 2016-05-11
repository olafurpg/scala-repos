// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.server

import java.io._

import akka.actor.SupervisorStrategy.Stop
import akka.actor._
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.ServerBinding
import akka.http.scaladsl.server.RouteResult.route2HandlerFlow
import akka.stream.ActorMaterializer
import akka.util.Timeout
import com.google.common.base.Charsets
import com.google.common.io.Files
import org.ensime.api._
import org.ensime.config._
import org.ensime.core._
import org.ensime.server.tcp.TCPServer
import org.slf4j._
import org.slf4j.bridge.SLF4JBridgeHandler

import scala.concurrent.duration._
import scala.util.Properties._
import scala.util._

case class ShutdownRequest(reason: String, isError: Boolean = false)

class ServerActor(
    config: EnsimeConfig,
    protocol: Protocol,
    interface: String = "127.0.0.1"
) extends Actor with ActorLogging {

  override val supervisorStrategy = OneForOneStrategy() {
    case ex: Exception =>
      log.error(s"Error with monitor actor ${ex.getMessage}", ex)
      self ! ShutdownRequest(s"Monitor actor failed with ${ex.getClass} - ${ex.toString}", isError = true)
      Stop
  }

  def initialiseChildren(): Unit = {

    implicit val config = this.config
    implicit val mat = ActorMaterializer()
    implicit val timeout = Timeout(10 seconds)

    val broadcaster = context.actorOf(Broadcaster(), "broadcaster")
    val project = context.actorOf(Project(broadcaster), "project")

    val preferredTcpPort = PortUtil.port(config.cacheDir, "port")
    val shutdownOnLastDisconnect = Environment.shutdownOnDisconnectFlag
    context.actorOf(Props(
      new TCPServer(
        config.cacheDir, protocol, project,
        broadcaster, shutdownOnLastDisconnect, preferredTcpPort
      )
    ), "tcp-server")

    // this is a bit ugly in a couple of ways
    // 1) web server creates handlers in the top domain
    // 2) We have to manually capture the failure to write the port file and lift the error to a failure.
    val webserver = new WebServerImpl(project, broadcaster)(config, context.system, mat, timeout)

    // async start the HTTP Server
    val selfRef = self
    val preferredHttpPort = PortUtil.port(config.cacheDir, "http")
    Http()(context.system).bindAndHandle(webserver.route, interface, preferredHttpPort.getOrElse(0)).onComplete {
      case Failure(ex) =>
        log.error(s"Error binding http endpoint ${ex.getMessage}", ex)
        selfRef ! ShutdownRequest(s"http endpoint failed to bind ($preferredHttpPort)", isError = true)

      case Success(ServerBinding(addr)) =>
        log.info(s"ENSIME HTTP on ${addr.getAddress}")
        try {
          PortUtil.writePort(config.cacheDir, addr.getPort, "http")
        } catch {
          case ex: Throwable =>
            log.error(s"Error initializing http endpoint ${ex.getMessage}", ex)
            selfRef ! ShutdownRequest(s"http endpoint failed to initialise: ${ex.getMessage}", isError = true)
        }
    }(context.system.dispatcher)

    Environment.info foreach log.info
  }

  override def preStart(): Unit = {
    try {
      initialiseChildren()
    } catch {
      case t: Throwable =>
        log.error(s"Error during startup - ${t.getMessage}", t)
        self ! ShutdownRequest(t.toString, isError = true)
    }
  }
  override def receive: Receive = {
    case req: ShutdownRequest =>
      triggerShutdown(req)
  }

  def triggerShutdown(request: ShutdownRequest): Unit = {
    Server.shutdown(context.system, request)
  }

}

object Server {
  SLF4JBridgeHandler.removeHandlersForRootLogger()
  SLF4JBridgeHandler.install()
  val log = LoggerFactory.getLogger("Server")

  def main(args: Array[String]): Unit = {
    val ensimeFileStr = propOrNone("ensime.config").getOrElse(
      throw new RuntimeException("ensime.config (the location of the .ensime file) must be set")
    )

    val ensimeFile = new File(ensimeFileStr)
    if (!ensimeFile.exists() || !ensimeFile.isFile)
      throw new RuntimeException(s".ensime file ($ensimeFile) not found")

    implicit val config = try {
      EnsimeConfigProtocol.parse(Files.toString(ensimeFile, Charsets.UTF_8))
    } catch {
      case e: Throwable =>
        log.error(s"There was a problem parsing $ensimeFile", e)
        throw e
    }

    val protocol: Protocol = propOrElse("ensime.protocol", "swank") match {
      case "swank" => new SwankProtocol
      case "jerk" => new JerkProtocol
      case other => throw new IllegalArgumentException(s"$other is not a valid ENSIME protocol")
    }

    val system = ActorSystem("ENSIME")
    system.actorOf(Props(new ServerActor(config, protocol)), "ensime-main")
  }

  def shutdown(system: ActorSystem, request: ShutdownRequest): Unit = {
    val t = new Thread(new Runnable {
      def run(): Unit = {
        if (request.isError)
          log.error(s"Shutdown requested due to internal error: ${request.reason}")
        else
          log.info(s"Shutdown requested: ${request.reason}")

        log.info("Shutting down the ActorSystem")
        Try(system.shutdown())

        log.info("Awaiting actor system termination")
        Try(system.awaitTermination())

        log.info("Shutdown complete")
        if (!propIsSet("ensime.server.test")) {
          if (request.isError)
            System.exit(1)
          else
            System.exit(0)
        }
      }
    })
    t.start()
  }
}
