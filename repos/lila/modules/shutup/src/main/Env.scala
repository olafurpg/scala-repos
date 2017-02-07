package lila.shutup

import akka.actor._
import com.typesafe.config.Config
import scala.concurrent.duration._

import lila.common.PimpedConfig._

final class Env(config: Config,
                reporter: akka.actor.ActorSelection,
                follows: (String, String) => Fu[Boolean],
                system: ActorSystem,
                db: lila.db.Env) {

  private val settings = new {
    val CollectionShutup = config getString "collection.shutup"
    val ActorName = config getString "actor.name"
  }
  import settings._

  lazy val api =
    new ShutupApi(coll = coll, follows = follows, reporter = reporter)

  private lazy val coll = db(CollectionShutup)

  // api actor
  system.actorOf(
    Props(new Actor {
      import lila.hub.actorApi.shutup._
      def receive = {
        case RecordPublicForumMessage(userId, text) =>
          api.publicForumMessage(userId, text)
        case RecordTeamForumMessage(userId, text) =>
          api.teamForumMessage(userId, text)
        case RecordPrivateMessage(userId, toUserId, text) =>
          api.privateMessage(userId, toUserId, text)
        case RecordPrivateChat(chatId, userId, text) =>
          api.privateChat(chatId, userId, text)
        case RecordPublicChat(chatId, userId, text) =>
          api.publicChat(chatId, userId, text)
      }
    }),
    name = ActorName
  )
}

object Env {

  lazy val current: Env =
    "shutup".boot(
      new Env(
        config = lila.common.PlayApp.loadConfig("shutup"),
        reporter = lila.hub.Env.current.actor.report,
        system = lila.common.PlayApp.system,
        follows = lila.relation.Env.current.api.fetchFollows _,
        db = lila.db.Env.current
      ))
}
