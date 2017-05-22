package lila.security

import scala.concurrent.Future

import org.joda.time.DateTime
import play.api.mvc.RequestHeader
import reactivemongo.bson.BSONDocument
import reactivemongo.bson.Macros

import lila.db.api._
import lila.db.BSON.BSONJodaDateTimeHandler
import lila.user.{User, UserRepo}
import lila.common.HTTPRequest
import tube.storeColl

object Store

  private[security] def save(sessionId: String,
                             userId: String,
                             req: RequestHeader,
                             apiVersion: Option[Int]): Funit =
    storeColl
      .insert(
          BSONDocument(
              "_id" -> sessionId,
              "user" -> userId,
              "ip" -> HTTPRequest.lastRemoteAddress(req),
              "ua" -> HTTPRequest.userAgent(req).|("?"),
              "date" -> DateTime.now,
              "up" -> true,
              "api" -> apiVersion
          ))
      .void

  def userId(sessionId: String): Fu[Option[String]] =
    storeColl
      .find(
          BSONDocument("_id" -> sessionId, "up" -> true),
          BSONDocument("user" -> true, "_id" -> false)
      )
      .one[BSONDocument] map { _ flatMap (_.getAs[String]("user")) }

  case class UserIdAndFingerprint(user: String, fp: Option[String])
  private implicit val UserIdAndFingerprintBSONReader =
    Macros.reader[UserIdAndFingerprint]

  def userIdAndFingerprint(
      sessionId: String): Fu[Option[UserIdAndFingerprint]] =
    storeColl
      .find(
          BSONDocument("_id" -> sessionId, "up" -> true),
          BSONDocument("user" -> true, "fp" -> true, "_id" -> false)
      )
      .one[UserIdAndFingerprint]

  def delete(sessionId: String): Funit =
    storeColl
      .update(BSONDocument("_id" -> sessionId),
              BSONDocument("$set" -> BSONDocument("up" -> false)))
      .void

  def closeUserAndSessionId(userId: String, sessionId: String): Funit =
    storeColl
      .update(BSONDocument("user" -> userId, "_id" -> sessionId, "up" -> true),
              BSONDocument("$set" -> BSONDocument("up" -> false)))
      .void

  def closeUserExceptSessionId(userId: String, sessionId: String): Funit =
    storeColl
      .update(BSONDocument("user" -> userId,
                           "_id" -> BSONDocument("$ne" -> sessionId),
                           "up" -> true),
              BSONDocument("$set" -> BSONDocument("up" -> false)),
              multi = true)
      .void

  // useful when closing an account,
  // we want to logout too
  def disconnect(userId: String): Funit =
    storeColl
      .update(BSONDocument("user" -> userId),
              BSONDocument("$set" -> BSONDocument("up" -> false)),
              multi = true)
      .void

  private implicit val UserSessionBSONHandler = Macros.handler[UserSession]
  def openSessions(userId: String, nb: Int): Fu[List[UserSession]] =
    storeColl
      .find(
          BSONDocument("user" -> userId, "up" -> true)
      )
      .sort(BSONDocument("date" -> -1))
      .cursor[UserSession]()
      .collect[List](nb)

  def setFingerprint(id: String, fingerprint: String): Fu[String] =
    import java.util.Base64
    import org.apache.commons.codec.binary.Hex
    scala.concurrent.Future
      Base64.getEncoder encodeToString
        Hex decodeHex fingerprint.toArray
      take 8
    flatMap  hash =>
      storeColl.update(
          BSONDocument("_id" -> id),
          BSONDocument("$set" -> BSONDocument("fp" -> hash))
      ) inject hash

  case class Info(ip: String, ua: String, fp: Option[String])
    def fingerprint = fp.map(_.toString)
  private implicit val InfoBSONHandler = Macros.handler[Info]

  def findInfoByUser(userId: String): Fu[List[Info]] =
    storeColl
      .find(
          BSONDocument("user" -> userId),
          BSONDocument(
              "_id" -> false, "ip" -> true, "ua" -> true, "fp" -> true)
      )
      .cursor[Info]()
      .collect[List]()

  private case class DedupInfo(_id: String, ip: String, ua: String)
    def compositeKey = s"$ip $ua"
  private implicit val DedupInfoBSONHandler = Macros.handler[DedupInfo]

  def dedup(userId: String, keepSessionId: String): Funit =
    storeColl
      .find(BSONDocument(
              "user" -> userId,
              "up" -> true
          ))
      .sort(BSONDocument("date" -> -1))
      .cursor[DedupInfo]()
      .collect[List]() flatMap  sessions =>
      val olds = sessions
        .groupBy(_.compositeKey)
        .values
        .map(_ drop 1)
        .flatten
        .filter(_._id != keepSessionId)
      storeColl
        .remove(
            BSONDocument("_id" -> BSONDocument("$in" -> olds.map(_._id)))
        )
        .void
