// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.server

import akka.actor._
import scala.reflect.ClassTag
import spray.json._

import akka.stream._
import akka.stream.scaladsl._
import akka.http.scaladsl.server._
import akka.http.scaladsl.model.ws._

/**
 * This crazy amount of indecipherable boilerplate is how we turn a
 * simple Actor into a Flow, without any backpressure, specifically
 * for use in akka-http WebSocket (but could be used elsewhere with
 * caution).
 *
 * If you want backpressure, you need to implement `Publisher` /
 * `Subscriber` or wait for:
 *
 *  - Source backpressure: https://github.com/akka/akka/issues/17610
 *  - Sink backpressure: https://github.com/akka/akka/issues/17967
 *
 * and then add appropriate Akka Streams buffering.
 *
 * Some more things that you might want:
 *
 *  - marshalling https://github.com/akka/akka/issues/17969
 *  - ping rate: https://github.com/akka/akka/issues/17968
 *  - compression: https://github.com/akka/akka/issues/17725
 *  - testing https://github.com/akka/akka/issues/17914
 *
 * And also note that WebSockets would almost certainly be better
 * modelled as a BidiFlow:
 *
 *  - https://github.com/akka/akka/issues/18008
 *
 * Some basic docs:
 *  http://doc.akka.io/docs/akka-stream-and-http-experimental/1.0-RC4/scala/http/routing-dsl/websocket-support.html
 */
object WebSocketBoilerplate {
  import Directives._

  /**
   * @param actor see `actorRefAsFlow`
   * @return a `Flow` suitable for use in a `Route`.
   */
  def jsonWebsocket[Incoming, Outgoing](
    actor: ActorRef => ActorRef
  )(
    implicit
    m1: RootJsonFormat[Incoming],
    m2: RootJsonFormat[Outgoing],
    mat: Materializer,
    oc: ClassTag[Outgoing],
    printer: JsonPrinter = PrettyPrinter
  ): Route = {
    val underlying = actorRefAsFlow[Incoming, Outgoing](actor)
    val marshalled = jsonMarshalledMessageFlow(underlying)
    handleWebsocketMessages(marshalled)
  }

  /**
   * @param actor constructor for an actor that accepts `Incoming` messages and
   *              sends (potentially async) `Outgoing` messages to
   *              `target` (the parameter).
   * @return the actor represented as a `Flow`, suitable for use in akka-stream
   *         with caution.
   */
  def actorRefAsFlow[Incoming, Outgoing](
    actor: ActorRef => ActorRef
  )(
    implicit
    mat: Materializer
  ): Flow[Incoming, Outgoing, Unit] = {
    val (target, pub) = Source.actorRef[Outgoing](
      0, OverflowStrategy.fail
    ).toMat(Sink.publisher)(Keep.both).run()
    val source = Source(pub)

    val handler = actor(target)
    val sink = Sink.actorRef[Incoming](handler, PoisonPill)

    Flow.wrap(sink, source)((_, _) => ())
  }

  /**
   * @param flow using user domain objects
   * @return a `Flow` using WebSocket `Message`s
   */
  def jsonMarshalledMessageFlow[Incoming, Outgoing](
    flow: Flow[Incoming, Outgoing, Unit]
  )(
    implicit
    m1: RootJsonFormat[Incoming],
    m2: RootJsonFormat[Outgoing],
    //mat: Materializer,
    oc: ClassTag[Outgoing],
    printer: JsonPrinter = PrettyPrinter
  ): Flow[Message, Message, Unit] = {
    Flow[Message].collect {
      case TextMessage.Strict(msg) =>
        msg.parseJson.convertTo[Incoming]
      case _ =>
        throw new IllegalArgumentException("not a valid message")
    }.via(flow).map {
      case e: Outgoing =>
        TextMessage.Strict(e.toJson.toString(printer)): Message
    }
  }

}
