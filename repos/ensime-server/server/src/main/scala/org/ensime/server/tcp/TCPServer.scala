// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.server.tcp

import java.io.File
import java.net.InetSocketAddress

import akka.actor.{ Actor, ActorLogging, ActorRef }
import akka.io.{ IO, Tcp }
import org.ensime.core.Protocol
import org.ensime.server.{ ShutdownRequest, PortUtil }

case object ClientConnectionClosed

class TCPServer(
    cacheDir: File,
    protocol: Protocol,
    project: ActorRef,
    broadcaster: ActorRef,
    shutdownOnLastDisconnect: Boolean,
    preferredPort: Option[Int]
) extends Actor with ActorLogging {

  import Tcp._
  import context.system

  var nextConnectionId = 1

  var activeConnections = 0

  IO(Tcp) ! Bind(self, new InetSocketAddress("127.0.0.1", preferredPort.getOrElse(0)))

  def receive = {
    case b @ Bound(localAddress) =>
      val boundPort = localAddress.getPort
      log.info(s"Bound server on port $boundPort")
      PortUtil.writePort(cacheDir, boundPort, "port")
    case CommandFailed(_: Bind) =>
      context.parent ! ShutdownRequest(s"TCP protocol failed to bind ($preferredPort)", isError = true)

    case ClientConnectionClosed =>
      activeConnections -= 1
      log.info("Client disconnected - active clients now: " + activeConnections)
      if (activeConnections == 0 && shutdownOnLastDisconnect) {
        log.info("Shutdown on last disconnect set - requesting server shutdown")
        context.parent ! ShutdownRequest("Last client disconnected and shtudownOnLastDisconnect set")
      }

    case c @ Connected(remote, local) =>
      log.info(s"Connection from " + remote.getHostName + "")
      val connectionId = nextConnectionId
      nextConnectionId += 1
      val connection = sender()
      val handler = context.actorOf(TCPConnectionActor(connection, protocol, project, broadcaster), s"con$connectionId")
      activeConnections += 1
      log.info("Client connected - active clients now: " + activeConnections)
      connection ! Register(handler)
  }
}
