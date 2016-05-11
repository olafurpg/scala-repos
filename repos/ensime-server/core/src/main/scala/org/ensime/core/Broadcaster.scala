// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import akka.actor._
import scala.collection.immutable.ListSet

/**
 * An actor that will forward all received messages onto actors that
 * have registered to receive messages. Messages wrapped with
 * `Persist` will be held and sent to all new subscribers (to cater
 * for legacy EnsimeEvents that older clients expect to see on
 * startup).
 *
 * To avoid memory leaks, non-restarting Actors should unregister when
 * they stop. Additional register requests for the same actor will
 * result in all Persist messages being sent again.
 *
 * It feels crazy to be writing this because it seems like something
 * that should be in a standard library:
 *
 * 1. the `EventStream` very nearly does what we want but doesn't
 *    retain information about the sender and uses method calls (not
 *    `tell`). NOTE: although undocumented, subscribing to a `trait`
 *    will subscribe to implementations.
 *
 * 2. the `PubSub` requires named subscriptions and requires non-`tell`
 *    boilerplate on both publisher and subscriber side (although has
 *    the advantage of better networked behaviour).
 *
 * 3. neither Akka solution has the concept of the "Persist" message.
 */
class Broadcaster extends Actor with ActorLogging {
  import Broadcaster.{ Register, Unregister, Persist }

  var subscribers = Set.empty[ActorRef]
  var persistant = ListSet.empty[(ActorRef, Any)] // preserves order

  def receive = {
    case Register =>
      subscribers += sender()
      persistant.foreach {
        case (originalSender, message) => sender() tell (message, originalSender)
      }

    case Unregister => subscribers -= sender()
    case Persist(message) =>
      persistant += ((sender(), message))
      send(message)
    case message =>
      send(message)
  }

  private def send(message: Any): Unit = for {
    subscriber <- subscribers
  } {
    subscriber forward message
  }
}

object Broadcaster {
  object Register
  object Unregister
  case class Persist(message: Any)

  def apply(): Props = Props(new Broadcaster)
}
