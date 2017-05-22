package views.html.puzzle

import play.api.libs.json.{JsArray, Json}
import play.twirl.api.Html

import controllers.routes
import lila.api.Context
import lila.app.templating.Environment._
import lila.puzzle._

object JsData extends lila.Steroids

  def history(infos: UserInfos) =
    Json.obj(
        "attempts" -> infos.history.map  a =>
      Json.obj("puzzleId" -> a.puzzleId,
               "date" -> a.date,
               "win" -> a.win,
               "time" -> a.time,
               "puzzleRating" -> a.puzzleRating,
               "puzzleRatingDiff" -> a.puzzleRatingDiff,
               "userRating" -> a.userRating,
               "userRatingDiff" -> a.userRatingDiff,
               "vote" -> a.vote)
    )

  def apply(puzzle: Puzzle,
            userInfos: Option[lila.puzzle.UserInfos],
            mode: String,
            animationDuration: scala.concurrent.duration.Duration,
            attempt: Option[Attempt] = None,
            win: Option[Boolean] = None,
            voted: Option[Boolean] = None)(implicit ctx: Context) =
    Json.obj(
        "puzzle" -> Json.obj(
            "id" -> puzzle.id,
            "rating" -> puzzle.perf.intRating,
            "attempts" -> puzzle.attempts,
            "fen" -> puzzle.fen,
            "color" -> puzzle.color.name,
            "initialMove" -> puzzle.initialMove,
            "initialPly" -> puzzle.initialPly,
            "gameId" -> puzzle.gameId,
            "lines" -> lila.puzzle.Line.toJson(puzzle.lines),
            "enabled" -> puzzle.enabled,
            "vote" -> puzzle.vote.sum,
            "url" -> s"$netBaseUrl${routes.Puzzle.show(puzzle.id)}"
        ),
        "pref" -> Json.obj(
            "coords" -> ctx.pref.coords
        ),
        "chessground" -> Json.obj(
            "highlight" -> Json.obj(
                "lastMove" -> ctx.pref.highlight,
                "check" -> ctx.pref.highlight
            ),
            "movable" -> Json.obj(
                "showDests" -> ctx.pref.destination
            ),
            "draggable" -> Json.obj(
                "showGhost" -> ctx.pref.highlight
            ),
            "premovable" -> Json.obj(
                "showDests" -> ctx.pref.destination
            )
        ),
        "animation" -> Json.obj(
            "duration" -> ctx.pref.animationFactor * animationDuration.toMillis
        ),
        "mode" -> mode,
        "attempt" -> attempt.map  a =>
          Json.obj(
              "userRatingDiff" -> a.userRatingDiff,
              "seconds" -> a.seconds,
              "win" -> a.win,
              "vote" -> a.vote
          )
        ,
        "win" -> win,
        "voted" -> voted,
        "user" -> userInfos.map  i =>
          Json.obj(
              "rating" -> i.user.perfs.puzzle.intRating,
              "history" -> i.history.nonEmpty.option(Json.toJson(i.chart))
          )
        ,
        "difficulty" -> ctx.isAuth.option
          Json.obj(
              "choices" -> JsArray(translatedDifficultyChoices.map
                case (k, v) => Json.arr(k, v)
              ),
              "current" -> ctx.pref.puzzleDifficulty
          )
        )
