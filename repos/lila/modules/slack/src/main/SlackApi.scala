package lila.slack

import lila.common.LightUser
import lila.hub.actorApi.slack._
import lila.user.User

final class SlackApi(
    client: SlackClient, implicit val lightUser: String => Option[LightUser])

  def donation(event: lila.hub.actorApi.DonationEvent): Funit =
    val user = event.userId flatMap lightUser
    val username = user.fold("Anonymous")(_.titleName)
    def amount(cents: Int) =
      s"$$${lila.common.Maths.truncateAt(cents / 100d, 2)}"
    client(
        SlackMessage(
            username = "donation",
            icon = "heart_eyes",
            text = s"$username donated ${amount(event.gross)} ($amount(
            event.net))! Weekly progress: ${event.progress}%",
            channel = "general"
        )) >> event.message.??  msg =>
      client(
          SlackMessage(username = username,
                       icon = "kissing_heart",
                       text = msg,
                       channel = "general"))

  def publishEvent(event: Event): Funit = event match
    case Error(msg) => publishError(msg)
    case Warning(msg) => publishWarning(msg)
    case Info(msg) => publishInfo(msg)
    case Victory(msg) => publishVictory(msg)

  def publishError(msg: String): Funit =
    client(
        SlackMessage(username = "lichess error",
                     icon = "lightning",
                     text = msg,
                     channel = "general"))

  def publishWarning(msg: String): Funit =
    client(
        SlackMessage(username = "lichess warning",
                     icon = "thinking_face",
                     text = msg,
                     channel = "general"))

  def publishVictory(msg: String): Funit =
    client(
        SlackMessage(username = "lichess victory",
                     icon = "tada",
                     text = msg,
                     channel = "general"))

  def publishInfo(msg: String): Funit =
    client(
        SlackMessage(username = "lichess info",
                     icon = "monkey",
                     text = msg,
                     channel = "general"))

  def userMod(user: User, mod: User): Funit =
    client(
        SlackMessage(
            username = mod.username,
            icon = "oncoming_police_car",
            text = s"Let's have a look at <http://lichess.org/@/${user.username}?mod>",
            channel = "tavern"))

  def deployPre: Funit =
    client(
        SlackMessage(
            username = "deployment",
            icon = "rocket",
            text = "Lichess will be updated in a few minutes! Fasten your seatbelts.",
            channel = "general"))

  def deployPost: Funit =
    client(
        SlackMessage(username = "deployment",
                     icon = "rocket",
                     text = "Lichess is being updated! Brace for impact.",
                     channel = "general"))
