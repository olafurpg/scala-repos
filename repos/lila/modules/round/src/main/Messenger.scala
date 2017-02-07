package lila.round

import akka.actor.ActorRef
import akka.actor.ActorSelection

import actorApi._
import lila.chat.actorApi._
import lila.game.Game
import lila.i18n.I18nKey.{Select => SelectI18nKey}
import lila.i18n.I18nKeys

final class Messenger(socketHub: akka.actor.ActorRef,
                      chat: ActorSelection,
                      i18nKeys: I18nKeys) {

  def system(game: Game, message: SelectI18nKey, args: Any*) {
    val translated = message(i18nKeys).en(args: _*)
    chat ! SystemTalk(game.id + "/w", translated, socketHub)
    if (game.nonAi) chat ! SystemTalk(game.id, translated, socketHub)
  }

  def systemForOwners(gameId: String, message: SelectI18nKey, args: Any*) {
    val translated = message(i18nKeys).en(args: _*)
    chat ! SystemTalk(gameId, translated, socketHub)
  }

  def watcher(gameId: String, member: Member, text: String, socket: ActorRef) =
    member.userId.foreach { userId =>
      chat ! UserTalk(gameId + "/w", userId, text, socket)
    }

  def owner(gameId: String, member: Member, text: String, socket: ActorRef) =
    chat !
      (member.userId match {
        case Some(userId) =>
          UserTalk(gameId, userId, text, socket, public = false)
        case None => PlayerTalk(gameId, member.color.white, text, socket)
      })
}
