package controllers

import play.api.data.Form
import play.api.mvc._, Results._

import lila.api.Context
import lila.app._
import lila.common.LilaCookie
import views._

object Pref extends LilaController {

  private def api = Env.pref.api
  private def forms = Env.pref.forms

  def form(categSlug: String) = Auth { implicit ctx => me =>
    lila.pref.PrefCateg(categSlug) match {
      case None => notFound
      case Some(categ) =>
        Ok(html.account.pref(me, forms prefOf ctx.pref, categ)).fuccess
    }
  }

  def formApply = AuthBody { implicit ctx => me =>
    implicit val req = ctx.body
    FormFuResult(forms.pref) { err =>
      fuccess(err.toString)
    } { data =>
      api.setPref(data(ctx.pref), notifyChange = true) inject Ok("saved")
    }
  }

  def miniFormApply = AuthBody { implicit ctx => me =>
    implicit val req = ctx.body
    FormFuResult(forms.miniPref) { err =>
      fuccess("nope")
    } { data =>
      api.setPref(data(ctx.pref), notifyChange = true) inject Ok("saved")
    }
  }

  def set(name: String) = OpenBody { implicit ctx =>
    implicit val req = ctx.body
    (setters get name) ?? {
      case (form, fn) =>
        FormResult(form) { v =>
          fn(v, ctx) map { cookie =>
            Ok(()).withCookies(cookie)
          }
        }
    }
  }

  def saveTag(name: String, value: String) = Auth { implicit ctx => me =>
    api.saveTag(me, name, value)
  }

  private lazy val setters = Map(
    "theme" -> (forms.theme -> save("theme") _),
    "pieceSet" -> (forms.pieceSet -> save("pieceSet") _),
    "theme3d" -> (forms.theme3d -> save("theme3d") _),
    "pieceSet3d" -> (forms.pieceSet3d -> save("pieceSet3d") _),
    "soundSet" -> (forms.soundSet -> save("soundSet") _),
    "bg" -> (forms.bg -> save("bg") _),
    "bgImg" -> (forms.bgImg -> save("bgImg") _),
    "is3d" -> (forms.is3d -> save("is3d") _)
  )

  private def save(name: String)(value: String, ctx: Context): Fu[Cookie] =
    ctx.me ?? {
      api.setPrefString(_, name, value, notifyChange = false)
    } inject LilaCookie.session(name, value)(ctx.req)
}
