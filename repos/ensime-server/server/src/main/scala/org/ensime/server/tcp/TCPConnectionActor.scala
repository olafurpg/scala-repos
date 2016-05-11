// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.server.tcp

import akka.actor._

import akka.actor.{ ActorRef, Props, ActorLogging, Actor }
import akka.io.Tcp
import akka.util.ByteString
import org.ensime.api.{ RpcRequestEnvelope, EnsimeServerError, RpcResponseEnvelope, EnsimeEvent }
import org.ensime.core.{ Broadcaster, Canonised, Protocol }
import org.ensime.server.RequestHandler

import scala.annotation.tailrec
import scala.util.control.NonFatal

class TCPConnectionActor(
    connection: ActorRef,
    protocol: Protocol,
    project: ActorRef,
    broadcaster: ActorRef
) extends Actor with Stash with ActorLogging {

  case object Ack extends Tcp.Event

  // sign death pact: this actor terminates when connection breaks
  context watch connection

  import Tcp._

  // bytes we have seen but have been unable to process yet
  var seen = ByteString()

  def handlePeerClosed(): Unit = {
    context.parent ! ClientConnectionClosed
    context stop self
  }

  override def receive: Receive = idle

  // not Receive, thanks to https://issues.scala-lang.org/browse/SI-8861
  // (fixed in 2.11.7)
  def idle: PartialFunction[Any, Unit] = incoming orElse readyToSend
  def busy: PartialFunction[Any, Unit] = incoming orElse awaitingAck

  def incoming: Receive = {
    case Received(data: ByteString) =>
      seen = seen ++ data
      attemptProcess()
    case PeerClosed =>
      handlePeerClosed()
  }

  def readyToSend: Receive = {
    case outgoing: EnsimeEvent =>
      sendMessage(RpcResponseEnvelope(None, outgoing))
    case outgoing: RpcResponseEnvelope =>
      sendMessage(outgoing)
  }

  def awaitingAck: Receive = {
    case Ack =>
      // we only stash outgoing messages, so this will cause them to be queued for sending
      unstashAll()
      context.become(idle, discardOld = true)
    case outgoing: EnsimeEvent =>
      stash()
    case outgoing: RpcResponseEnvelope =>
      stash()
    case CommandFailed(Write(_, _)) =>
      connection ! ResumeWriting
  }

  def sendMessage(envelope: RpcResponseEnvelope): Unit = {
    val msg = try {
      protocol.encode(envelope)
    } catch {
      case NonFatal(t) =>
        log.error(t, s"Problem serialising $envelope")
        protocol.encode(
          RpcResponseEnvelope(
            envelope.callId,
            EnsimeServerError(s"Server error: ${t.getMessage}")
          )
        )
    }
    connection ! Tcp.Write(msg, Ack)
    context.become(busy, discardOld = true)
  }

  override def preStart(): Unit = {
    broadcaster ! Broadcaster.Register
  }

  final def attemptProcess(): Unit = {
    try {
      repeatedDecode()
    } catch {
      case e: Throwable =>
        log.error(e, "Error seen during message processing, closing client connection")
        context.stop(self)
    }

  }

  @tailrec
  final def repeatedDecode(): Unit = {
    val (envelopeOpt, remainder) = protocol.decode(seen)
    seen = remainder
    envelopeOpt match {
      case Some(rawEnvelope: RpcRequestEnvelope) =>
        val envelope = Canonised(rawEnvelope)
        context.actorOf(RequestHandler(envelope, project, self), s"${envelope.callId}")
        repeatedDecode()
      case None =>
    }
  }
}

object TCPConnectionActor {
  def apply(connection: ActorRef, protocol: Protocol, project: ActorRef, broadcaster: ActorRef): Props =
    Props(new TCPConnectionActor(connection, protocol, project, broadcaster))
}
