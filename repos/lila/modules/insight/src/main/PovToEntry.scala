package lila.insight

import chess.format.Nag
import chess.{Role, Board}
import lila.analyse.{Accuracy, Advice}
import lila.game.{Game, Pov, GameRepo}
import lila.user.User
import scalaz.NonEmptyList

object PovToEntry {

  private type Ply = Int

  case class RichPov(pov: Pov,
                     provisional: Boolean,
                     initialFen: Option[String],
                     analysis: Option[lila.analyse.Analysis],
                     division: chess.Division,
                     moveAccuracy: Option[List[Int]],
                     boards: NonEmptyList[Board],
                     movetimes: NonEmptyList[Int],
                     advices: Map[Ply, Advice])

  def apply(game: Game,
            userId: String,
            provisional: Boolean): Fu[Either[Game, Entry]] =
    enrich(game, userId, provisional) map (_ flatMap convert toRight game) addFailureEffect {
      e =>
        println(s"http://l.org/${game.id}")
    }

  private def removeWrongAnalysis(game: Game): Boolean = {
    if (game.metadata.analysed && !game.analysable) {
      GameRepo setUnanalysed game.id
      lila.analyse.AnalysisRepo remove game.id
      true
    }
    false
  }

  private def enrich(game: Game,
                     userId: String,
                     provisional: Boolean): Fu[Option[RichPov]] =
    if (removeWrongAnalysis(game)) fuccess(none)
    else
      lila.game.Pov.ofUserId(game, userId) ?? { pov =>
        lila.game.GameRepo.initialFen(game) zip
          (game.metadata.analysed ?? lila.analyse.AnalysisRepo
            .byId(game.id)) map {
          case (fen, an) =>
            for {
              boards <- chess.Replay
                         .boards(moveStrs = game.pgnMoves,
                                 initialFen = fen,
                                 variant = game.variant)
                         .toOption
                         .flatMap(_.toNel)
              movetimes <- game.moveTimes(pov.color).toNel
            } yield
              RichPov(
                  pov = pov,
                  provisional = provisional,
                  initialFen = fen,
                  analysis = an,
                  division = chess.Divider(boards.list),
                  moveAccuracy = an.map { Accuracy.diffsList(pov, _) },
                  boards = boards,
                  movetimes = movetimes,
                  advices = an.?? {
                    _.advices.map { a =>
                      a.info.ply -> a
                    }.toMap
                  }
              )
        }
      }

  private def pgnMoveToRole(pgn: String): Role = pgn.head match {
    case 'N' => chess.Knight
    case 'B' => chess.Bishop
    case 'R' => chess.Rook
    case 'Q' => chess.Queen
    case 'K' | 'O' => chess.King
    case _ => chess.Pawn
  }

  private def makeMoves(from: RichPov): List[Move] = {
    val cpDiffs = ~from.moveAccuracy toVector
    val prevInfos = from.analysis.?? { an =>
      Accuracy.prevColorInfos(from.pov, an) |> { is =>
        from.pov.color.fold(is, is.map(_.invert))
      }
    }
    val movetimes = from.movetimes.list
    val roles = from.pov.game.pgnMoves(from.pov.color) map pgnMoveToRole
    val boards = {
      val pivot = if (from.pov.color == from.pov.game.startColor) 0 else 1
      from.boards.list.zipWithIndex.collect {
        case (e, i) if (i % 2) == pivot => e
      }
    }
    movetimes.zip(roles).zip(boards).zipWithIndex.map {
      case (((tenths, role), board), i) =>
        val ply = i * 2 + from.pov.color.fold(1, 2)
        val prevInfo = prevInfos lift i
        val opportunism =
          from.advices.get(ply - 1) flatMap {
            case o if o.nag == Nag.Blunder =>
              from.advices get ply match {
                case Some(p) if p.nag == Nag.Blunder => false.some
                case _ => true.some
              }
            case _ => none
          }
        val luck =
          from.advices.get(ply) flatMap {
            case o if o.nag == Nag.Blunder =>
              from.advices.get(ply + 1) match {
                case Some(p) if p.nag == Nag.Blunder => true.some
                case _ => false.some
              }
            case _ => none
          }
        Move(phase = Phase.of(from.division, ply),
             tenths = tenths,
             role = role,
             eval = prevInfo.flatMap(_.score).map(_.ceiled.centipawns),
             mate = prevInfo.flatMap(_.mate),
             cpl = cpDiffs lift i map (_ min 1000),
             material = board.materialImbalance * from.pov.color.fold(1, -1),
             opportunism = opportunism,
             luck = luck)
    }
  }

  private def queenTrade(from: RichPov) = QueenTrade {
    from.division.end
      .fold(from.boards.last.some)(from.boards.list.lift) match {
      case Some(board) =>
        chess.Color.all.forall { color =>
          !board.hasPiece(chess.Piece(color, chess.Queen))
        }
      case _ =>
        logger.warn(s"http://l.org/${from.pov.game.id} missing endgame board")
        false
    }
  }

  private def convert(from: RichPov): Option[Entry] = {
    import from._
    for {
      myId <- pov.player.userId
      myRating <- pov.player.rating
      opRating <- pov.opponent.rating
      perfType <- pov.game.perfType
    } yield
      Entry(
          id = Entry povToId pov,
          number = 0, // temporary :-/ the Indexer will set it
          userId = myId,
          color = pov.color,
          perf = perfType,
          eco = Ecopening fromGame pov.game,
          myCastling = Castling.fromMoves(pov.game pgnMoves pov.color),
          opponentRating = opRating,
          opponentStrength = RelativeStrength(opRating - myRating),
          opponentCastling = Castling.fromMoves(pov.game pgnMoves !pov.color),
          moves = makeMoves(from),
          queenTrade = queenTrade(from),
          result = pov.game.winnerUserId match {
            case None => Result.Draw
            case Some(u) if u == myId => Result.Win
            case _ => Result.Loss
          },
          termination = Termination fromStatus pov.game.status,
          ratingDiff = ~pov.player.ratingDiff,
          analysed = analysis.isDefined,
          provisional = provisional,
          date = pov.game.createdAt)
  }
}
