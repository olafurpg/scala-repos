package lila.fishnet

import org.joda.time.DateTime

import chess.format.{FEN, Forsyth}

import lila.game.{Game, GameRepo, UciMemo}

final class Player(moveDb: MoveDB, uciMemo: UciMemo) {

  val maxPlies = 300

  def apply(game: Game): Funit = game.aiLevel ?? { level =>
    makeWork(game, level).map { move =>
      if (!moveDb.exists(_.similar(move))) moveDb.add(move)
    }
  }

  private def makeWork(game: Game, level: Int): Fu[Work.Move] =
    if (game.toChess.situation.playable(true))
      if (game.turns <= maxPlies)
        GameRepo.initialFen(game).zip(uciMemo.get(game)).map {
          case (initialFen, moves) =>
            Work.Move(
              _id = Work.makeId,
              game = Work.Game(id = game.id,
                               initialFen = initialFen.map(FEN.apply),
                               variant = game.variant,
                               moves = moves mkString " "),
              currentFen = FEN(Forsyth >> game.toChess),
              level = level,
              tries = 0,
              acquired = None,
              createdAt = DateTime.now
            )
        } else
        fufail(
          s"[fishnet] Too many moves (${game.turns}), won't play ${game.id}")
    else fufail("[fishnet] invalid position")
}
