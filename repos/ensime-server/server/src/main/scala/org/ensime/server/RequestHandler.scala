// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.server

import akka.actor._
import akka.event.LoggingReceive
import org.ensime.api._
import org.ensime.core._

/**
 * Spawned to listen to the server's response to an RpcRequest.
 */
class RequestHandler(
    envelope: RpcRequestEnvelope,
    project: ActorRef,
    server: ActorRef
) extends Actor with ActorLogging {

  override def preStart(): Unit = {
    log.debug(envelope.req.toString)
    envelope.req match {
      // multi-phase queries
      case DocUriAtPointReq(_, _) | DocUriForSymbolReq(_, _, _) =>
        project ! envelope.req
        context.become(resolveDocSig, discardOld = false)

      case req => project ! req
    }
  }

  def resolveDocSig: Receive = LoggingReceive.withLabel("resolveDocSig") {
    case None =>
      self ! FalseResponse
      context.unbecome()
    case Some(sig: DocSigPair) =>
      project ! sig
      context.unbecome()
  }

  // we can put all manner of timeout / monitoring logic in here

  def receive = LoggingReceive.withLabel("receive") {
    case err: EnsimeServerError =>
      server forward RpcResponseEnvelope(Some(envelope.callId), err)
      context stop self

    case response: RpcResponse =>
      server forward RpcResponseEnvelope(Some(envelope.callId), response)
      context stop self
  }

}
object RequestHandler {
  def apply(
    env: RpcRequestEnvelope,
    project: ActorRef,
    server: ActorRef
  ): Props = Props(classOf[RequestHandler], env, project, server)
}
