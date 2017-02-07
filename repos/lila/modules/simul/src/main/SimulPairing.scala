package lila.simul

import lila.game.{PovRef, IdGenerator}

case class SimulPairing(player: SimulPlayer,
                        gameId: String,
                        status: chess.Status,
                        wins: Option[Boolean],
                        hostColor: chess.Color) {

  def finished = status >= chess.Status.Mate

  def is(userId: String): Boolean = player.is(userId)
  def is(other: SimulPlayer): Boolean = player.is(other)

  def finish(s: chess.Status, w: Option[String], t: Int) =
    copy(status = s, wins = w.map(player.is))

  def winnerColor = wins.map { w =>
    if (w) !hostColor else hostColor
  }
}

private[simul] object SimulPairing {

  def apply(player: SimulPlayer): SimulPairing =
    new SimulPairing(player = player,
                     gameId = IdGenerator.game,
                     status = chess.Status.Created,
                     wins = none,
                     hostColor = chess.White)
}
