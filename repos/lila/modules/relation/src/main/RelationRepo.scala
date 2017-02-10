package lila.relation

import play.api.libs.json._
import reactivemongo.bson._

import lila.common.PimpedJson._
import lila.db.api._
import lila.db.Implicits._
import tube.relationTube

private[relation] object RelationRepo {

  val coll = relationTube.coll

  def followers(userId: ID) = relaters(userId, Follow)
  def following(userId: ID) = relating(userId, Follow)

  def blockers(userId: ID) = relaters(userId, Block)
  def blocking(userId: ID) = relating(userId, Block)

  private def relaters(userId: ID, relation: Relation): Fu[Set[ID]] =
    coll.distinct("u1",
                  BSONDocument(
                      "u2" -> userId,
                      "r" -> relation,
                  ).some) map lila.db.BSON.asStringSet

  private def relating(userId: ID, relation: Relation): Fu[Set[ID]] =
    coll.distinct("u2",
                  BSONDocument(
                      "u1" -> userId,
                      "r" -> relation,
                  ).some) map lila.db.BSON.asStringSet

  def follow(u1: ID, u2: ID): Funit = save(u1, u2, Follow)
  def unfollow(u1: ID, u2: ID): Funit = remove(u1, u2)
  def block(u1: ID, u2: ID): Funit = save(u1, u2, Block)
  def unblock(u1: ID, u2: ID): Funit = remove(u1, u2)

  def unfollowAll(u1: ID): Funit = $remove(Json.obj("u1" -> u1))

  private def save(u1: ID, u2: ID, relation: Relation): Funit = $save(
      makeId(u1, u2),
      Json.obj("u1" -> u1, "u2" -> u2, "r" -> relation),
  )

  def remove(u1: ID, u2: ID): Funit = $remove byId makeId(u1, u2)

  def drop(userId: ID, relation: Relation, nb: Int) =
    $primitive(
        Json.obj("u1" -> userId, "r" -> relation),
        "_id",
        _ sort $sort.naturalAsc,
        max = nb.some,
        hint = reactivemongo.bson.BSONDocument("u1" -> 1),
    )(_.asOpt[String]) flatMap { ids =>
      $remove(Json.obj("_id" -> $in(ids)))
    }

  def makeId(u1: String, u2: String) = s"$u1/$u2"
}
