// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.server

import akka.actor._
import akka.pattern.ask
import akka.stream.Materializer
import akka.util.Timeout
import org.ensime.api._
import org.ensime.core._
import scala.concurrent.Future

class WebServerImpl(
    project: ActorRef,
    broadcaster: ActorRef
)(
    implicit
    val config: EnsimeConfig,
    val system: ActorSystem,
    val mat: Materializer,
    val timeout: Timeout
) extends WebServer with DocJarReading {
  import system.dispatcher

  private def handleRpc(in: Any): Future[EnsimeServerMessage] = in match {
    case DocUriAtPointReq(_, _) | DocUriForSymbolReq(_, _, _) =>
      (project ? Canonised(in)).flatMap {
        case None => Future.successful(FalseResponse)
        case Some(sig: DocSigPair) => handleRpc(sig)
      }
    case _ => (project ? Canonised(in)).mapTo[EnsimeServerMessage]
  }

  def restHandler(in: RpcRequest): Future[EnsimeServerMessage] = handleRpc(in)

  def websocketHandler(target: ActorRef): ActorRef = {
    system.actorOf(ConnectionHandler(project, broadcaster, target))
  }

}
