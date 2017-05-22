package lila.challenge

import akka.actor._
import org.joda.time.DateTime
import scala.concurrent.duration._

import lila.game.{Game, Pov, GameRepo}
import lila.hub.actorApi.map.Tell
import lila.hub.actorApi.SendTo
import lila.memo.{MixedCache, AsyncCache}
import lila.user.{User, UserRepo}

final class ChallengeApi(repo: ChallengeRepo,
                         joiner: Joiner,
                         jsonView: JsonView,
                         socketHub: ActorRef,
                         userRegister: ActorSelection,
                         lilaBus: lila.common.Bus)

  import Challenge._

  def allFor(userId: User.ID): Fu[AllChallenges] =
    createdByDestId(userId) zip createdByChallengerId(userId) map (AllChallenges.apply _).tupled

  def create(c: Challenge): Funit =
    repo like c flatMap { _ ?? repo.cancel }
  >> (repo insert c) >> uncacheAndNotify(c) >>-
    lilaBus.publish(Event.Create(c), 'challenge)

  def byId = repo byId _

  val countInFor = AsyncCache(repo.countCreatedByDestId, maxCapacity = 20000)

  def createdByChallengerId = repo createdByChallengerId _

  def createdByDestId = repo createdByDestId _

  def cancel(c: Challenge) = (repo cancel c) >> uncacheAndNotify(c)

  private def offline(c: Challenge) = (repo offline c) >> uncacheAndNotify(c)

  private[challenge] def ping(id: Challenge.ID): Funit =
    repo statusById id flatMap
      case Some(Status.Created) => repo setSeen id
      case Some(Status.Offline) =>
        (repo setSeenAgain id) >> byId(id).flatMap { _ ?? uncacheAndNotify }
      case _ => fuccess(socketReload(id))

  def decline(c: Challenge) = (repo decline c) >> uncacheAndNotify(c)

  def accept(c: Challenge, user: Option[User]): Fu[Option[Pov]] =
    joiner(c, user).flatMap
      case None => fuccess(None)
      case Some(pov) =>
        (repo accept c) >> uncacheAndNotify(c) >>-
          lilaBus.publish(Event.Accept(c, user.map(_.id)), 'challenge)
        inject pov.some

  def rematchOf(game: Game, user: User): Fu[Boolean] =
    Pov.ofUserId(game, user.id) ??  pov =>
      for
        initialFen <- GameRepo initialFen pov.game
        challengerOption <- pov.player.userId ?? UserRepo.byId
        destUserOption <- pov.opponent.userId ?? UserRepo.byId
        success <- (destUserOption |@| challengerOption).tupled ??
          case (destUser, challenger) =>
            create(
                Challenge.make(
                    variant = pov.game.variant,
                    initialFen = initialFen,
                    timeControl = (pov.game.clock, pov.game.daysPerTurn) match
                  case (Some(clock), _) =>
                    TimeControl.Clock(clock.limit, clock.increment)
                  case (_, Some(days)) => TimeControl.Correspondence(days)
                  case _ => TimeControl.Unlimited
                ,
                    mode = pov.game.mode,
                    color = (!pov.color).name,
                    challenger = Right(challenger),
                    destUser = Some(destUser),
                    rematchOf = pov.game.id.some
                )) inject true
      yield success

  def removeByUserId = repo removeByUserId _

  private[challenge] def sweep: Funit =
    repo.realTimeUnseenSince(DateTime.now minusSeconds 10, max = 50).flatMap
      cs =>
        lila.common.Future.applySequentially(cs)(offline).void
    >> repo.expiredIds(max = 50).flatMap  ids =>
      lila.common.Future.applySequentially(ids)(remove).void

  private def remove(id: Challenge.ID) =
    repo.remove(id) >> countInFor.remove(id)

  private def uncacheAndNotify(c: Challenge) =
    (c.destUserId ?? countInFor.remove) >>- (c.destUserId ?? notify) >>-
    (c.challengerUserId ?? notify) >>- socketReload(c.id)

  private def socketReload(id: Challenge.ID)
    socketHub ! Tell(id, Socket.Reload)

  private def notify(userId: User.ID)
    allFor(userId) foreach  all =>
      userRegister ! SendTo(
          userId, lila.socket.Socket.makeMessage("challenges", jsonView(all)))
