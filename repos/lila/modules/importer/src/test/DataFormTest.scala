package lila.importer

import chess._
import chess.Pos._
import org.specs2.mutable._
import org.specs2.specification._
import ornicar.scalalib.test._

class DataFormTest extends Specification with ScalazValidationMatchers

  "Import from position" should
    "preserve initial FEN" in
      val pgn =
        """[Event "Casual game"]
[Result "1-0"]
[PlyCount "9"]
[Variant "From Position"]
[Termination "Normal"]
[FEN "rk6/p1r3p1/P3B1K1/1p2B3/8/8/8/8 w - - 0 1"]
[SetUp "1"]

1. Bd7 b4 2. Kf7 b3 3. Ke8 b2 4. Kd8 g6 5. Bxc7# { Black is checkmated } 1-0"""

      ImportData(pgn, None).preprocess(None) must beSuccess.like
        case s => s.game.toChess.board.history.castles must_== Castles.none
