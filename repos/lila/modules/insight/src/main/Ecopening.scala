package lila.insight

case class Ecopening(eco: Ecopening.ECO,
                     family: Ecopening.FamilyName,
                     name: String,
                     private val moves: String,
                     fen: Ecopening.FEN,
                     lastMoveUci: String)
    extends Ordered[Ecopening] {

  private lazy val moveList = moves.split(' ').toList

  def firstMove = moveList.headOption

  lazy val size = moveList.size

  lazy val formattedMoves: String = moveList
    .grouped(2)
    .zipWithIndex
    .map {
      case (List(w, b), i) => s"${i + 1}. $w $b"
      case (List(w), i) => s"${i + 1}. $w"
      case _ => ""
    }
    .mkString(" ")

  def ecoName = s"$eco $name"

  def compare(other: Ecopening) = eco compare other.eco

  override def toString = s"$ecoName ($moves)"
}

object Ecopening {
  type FamilyName = String
  type ECO = String
  type FEN = String

  case class Family(name: FamilyName, ecos: List[FEN])
  def makeFamilies(ops: Iterable[Ecopening]): Map[FamilyName, Family] =
    ops.foldLeft(Map.empty[FamilyName, Family]) {
      case (fams, op) =>
        fams +
        (op.family -> fams
              .get(op.family)
              .fold(Family(op.family, List(op.eco))) { existing =>
                existing.copy(ecos = op.eco :: existing.ecos)
              })
    }

  def fromGame(game: lila.game.Game): Option[Ecopening] =
    if (game.playable || game.turns < 4 || game.fromPosition ||
        game.variant.exotic) none
    else
      chess.Replay
        .boards(
            moveStrs = game.pgnMoves take EcopeningDB.MAX_MOVES,
            initialFen = none,
            variant = chess.variant.Standard,
        )
        .toOption flatMap matchChronoBoards

  private def matchChronoBoards(boards: List[chess.Board]): Option[Ecopening] =
    boards.reverse.foldLeft(none[Ecopening]) {
      case (acc, board) =>
        acc orElse {
          EcopeningDB.allByFen get chess.format.Forsyth.exportBoard(board)
        }
    }
}
