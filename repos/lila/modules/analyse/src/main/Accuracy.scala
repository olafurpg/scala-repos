package lila.analyse

import lila.game.Pov

object Accuracy {

  def withSignOf(i: Int, signed: Int) = if (signed < 0) -i else i

  private val makeDiff: PartialFunction[(Option[Score],
                                         Option[Int],
                                         Option[Score],
                                         Option[Int]),
                                        Int] = {
    case (Some(s1), _, Some(s2), _) =>
      s2.ceiled.centipawns - s1.ceiled.centipawns
    case (Some(s1), _, _, Some(m2)) =>
      withSignOf(Score.CEILING, m2) - s1.ceiled.centipawns
    case (_, Some(m1), Some(s2), _) =>
      s2.ceiled.centipawns - withSignOf(Score.CEILING, m1)
    case (_, Some(m1), _, Some(m2)) =>
      withSignOf(Score.CEILING, m2) - withSignOf(Score.CEILING, m1)
  }

  def diffsList(pov: Pov, analysis: Analysis): List[Int] =
    (pov.color == pov.game.startColor)
      .fold(
          Info.start(pov.game.startedAtTurn) :: analysis.infos,
          analysis.infos
      )
      .grouped(2)
      .foldLeft(List[Int]()) {
        case (list, List(i1, i2)) =>
          makeDiff.lift(i1.score, i1.mate, i2.score, i2.mate).fold(list) {
            diff =>
              (if (pov.color.white) -diff else diff).max(0) :: list
          }
        case (list, _) => list
      }
      .reverse

  def prevColorInfos(pov: Pov, analysis: Analysis): List[Info] = {
    (pov.color == pov.game.startColor)
      .fold(
          Info.start(pov.game.startedAtTurn) :: analysis.infos,
          analysis.infos
      )
      .zipWithIndex
      .collect {
        case (e, i) if (i % 2) == 0 => e
      }
  }

  def mean(pov: Pov, analysis: Analysis): Option[Int] = {
    val diffs = diffsList(pov, analysis)
    val nb = diffs.size
    (nb != 0) option (diffs.sum / nb)
  }
}
