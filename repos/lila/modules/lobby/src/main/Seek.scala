package lila.lobby

import chess.{Mode, Clock, Speed}
import org.joda.time.DateTime
import ornicar.scalalib.Random
import play.api.libs.json._

import actorApi.LobbyUser
import lila.game.PerfPicker
import lila.rating.RatingRange
import lila.user.{User, Perfs}

// correspondence chess, persistent
case class Seek(_id: String,
                variant: Int,
                daysPerTurn: Option[Int],
                mode: Int,
                color: String,
                user: LobbyUser,
                ratingRange: String,
                createdAt: DateTime) {

  def id = _id

  val realColor = Color.orDefault(color)

  val realVariant = chess.variant.Variant.orDefault(variant)

  val realMode = Mode.orDefault(mode)

  def compatibleWith(h: Seek) =
    user.id != h.user.id &&
      compatibilityProperties == h.compatibilityProperties &&
      (realColor
        .compatibleWith(h.realColor)) && ratingRangeCompatibleWith(h) &&
      h.ratingRangeCompatibleWith(this)

  private def ratingRangeCompatibleWith(h: Seek) = realRatingRange.fold(true) {
    range =>
      h.rating ?? range.contains
  }

  private def compatibilityProperties = (variant, mode, daysPerTurn)

  lazy val realRatingRange: Option[RatingRange] =
    RatingRange noneIfDefault ratingRange

  def rating = perfType.map(_.key).flatMap(user.ratingMap.get)

  def render: JsObject = Json.obj(
    "id" -> _id,
    "username" -> user.username,
    "rating" -> rating,
    "variant" -> Json.obj("key" -> realVariant.key,
                          "short" -> realVariant.shortName,
                          "name" -> realVariant.name),
    "mode" -> realMode.id,
    "days" -> daysPerTurn,
    "color" -> chess.Color(color).??(_.name),
    "perf" -> Json.obj("icon" -> perfType.map(_.iconChar.toString),
                       "name" -> perfType.map(_.name))
  )

  lazy val perfType =
    PerfPicker.perfType(Speed.Correspondence, realVariant, daysPerTurn)
}

object Seek {

  val idSize = 8

  def make(variant: chess.variant.Variant,
           daysPerTurn: Option[Int],
           mode: Mode,
           color: String,
           user: User,
           ratingRange: RatingRange,
           blocking: Set[String]): Seek =
    new Seek(
      _id = Random nextStringUppercase idSize,
      variant = variant.id,
      daysPerTurn = daysPerTurn,
      mode = mode.id,
      color = color,
      user = LobbyUser.make(user, blocking),
      ratingRange = ratingRange.toString,
      createdAt = DateTime.now
    )

  def renew(seek: Seek) =
    new Seek(
      _id = Random nextStringUppercase idSize,
      variant = seek.variant,
      daysPerTurn = seek.daysPerTurn,
      mode = seek.mode,
      color = seek.color,
      user = seek.user,
      ratingRange = seek.ratingRange,
      createdAt = DateTime.now
    )

  import reactivemongo.bson.Macros
  import lila.db.BSON.MapValue.MapHandler
  import lila.db.BSON.BSONJodaDateTimeHandler
  private[lobby] implicit val lobbyUserBSONHandler = Macros.handler[LobbyUser]
  private[lobby] implicit val seekBSONHandler = Macros.handler[Seek]
}
