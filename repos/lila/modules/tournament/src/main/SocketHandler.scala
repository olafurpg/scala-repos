package lila.tournament

import scala.concurrent.duration._

import akka.actor._
import akka.pattern.ask

import actorApi._
import lila.common.PimpedJson._
import lila.hub.actorApi.map._
import lila.security.Flood
import akka.actor.ActorSelection
import lila.socket.actorApi.{Connected => _, _}
import lila.socket.Handler
import lila.user.User
import makeTimeout.short

private[tournament] final class SocketHandler(hub: lila.hub.Env,
                                              socketHub: ActorRef,
                                              chat: ActorSelection,
                                              flood: Flood) {

  def join(tourId: String,
           uid: String,
           user: Option[User]): Fu[Option[JsSocketHandler]] =
    TournamentRepo.exists(tourId) flatMap {
      _ ?? {
        for {
          socket ← socketHub ? Get(tourId) mapTo manifest[ActorRef]
          join = Join(uid = uid, user = user)
          handler ← Handler(hub, socket, uid, join, user map (_.id)) {
                     case Connected(enum, member) =>
                       (controller(socket, tourId, uid, member), enum, member)
                   }
        } yield handler.some
      }
    }

  private def controller(socket: ActorRef,
                         tourId: String,
                         uid: String,
                         member: Member): Handler.Controller = {
    case ("p", o) =>
      o int "v" foreach { v =>
        socket ! PingVersion(uid, v)
      }
    case ("talk", o) =>
      o str "d" foreach { text =>
        member.userId foreach { userId =>
          chat ! lila.chat.actorApi.UserTalk(tourId, userId, text, socket)
        }
      }
  }
}
