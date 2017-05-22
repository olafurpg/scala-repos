package lila.fishnet

import chess.format.pgn.Dumper
import chess.format.Uci
import chess.{Replay, Move, Situation}
import scalaz.Validation.FlatMap._

import lila.analyse.{Analysis, Info, PgnMove}
import lila.common.LilaException

// convert variations from UCI to PGN.
// also drops extra variations
private object UciToPgn

  type WithErrors[A] = (A, List[Exception])

  def apply(replay: Replay, analysis: Analysis): WithErrors[Analysis] =

    val pliesWithAdviceAndVariation = (analysis.advices collect
          case a if a.info.hasVariation => a.ply
        ).toSet

    val onlyMeaningfulVariations: List[Info] =
      analysis.infos map  info =>
        if (pliesWithAdviceAndVariation(info.ply)) info
        else info.dropVariation

    def uciToPgn(ply: Int, variation: List[String]): Valid[List[PgnMove]] =
      for
        situation ← if (ply == replay.setup.startedAtTurn + 1)
                     success(replay.setup.situation)
                   else
                     replay moveAtPly ply map
                     (_.fold(_.situationBefore, _.situationBefore)) toValid "No move found"
        ucis ← variation.map(Uci.Move.apply).sequence toValid "Invalid UCI moves " +
        variation
        moves ← ucis.foldLeft[Valid[(Situation, List[Move])]](
            success(situation -> Nil))
          case (scalaz.Success((sit, moves)), uci) =>
            sit.move(uci.orig, uci.dest, uci.promotion) prefixFailuresWith s"ply $ply " map
              move =>
                move.situationAfter -> (move :: moves)
          case (failure, _) => failure
      yield moves._2.reverse map Dumper.apply

    onlyMeaningfulVariations.foldLeft[WithErrors[List[Info]]]((Nil, Nil))
      case ((infos, errs), info) if info.variation.isEmpty =>
        (info :: infos, errs)
      case ((infos, errs), info) =>
        uciToPgn(info.ply, info.variation).fold(
            err => (info.dropVariation :: infos, LilaException(err) :: errs),
            pgn => (info.copy(variation = pgn) :: infos, errs)
        )
    match
      case (infos, errors) => analysis.copy(infos = infos.reverse) -> errors
