package controllers

import play.api._
import play.api.data.Form
import play.api.mvc._

import lila.api.Context
import lila.app._
import lila.qa.{QuestionId, Question, AnswerId, Answer, QaAuth}

trait QaController extends LilaController {

  protected def api = Env.qa.api
  protected def forms = Env.qa.forms

  protected def renderQuestion(
      q: Question, answerForm: Option[Form[_]] = None)(
      implicit ctx: Context): Fu[Result] =
    (api.answer popular q.id) zip fetchPopular zip api.relation.questions(
        q, 10) zip
    (QaAuth.canAsk ?? { forms.anyCaptcha map (_.some) }) flatMap {
      case (((answers, popular), related), captcha) =>
        fuccess {
          Ok(
              views.html.qa.questionShow(
                  q,
                  answers,
                  popular,
                  related,
                  answerForm =
                    if (QaAuth canAnswer q)
                      answerForm orElse Some(forms.answer)
                    else None,
                  captcha = captcha))
        }
      case _ => notFound
    }

  protected def renderN00b(implicit ctx: Context) = fetchPopular map {
    popular =>
      Forbidden(views.html.qa.n00b(popular))
  }

  protected def WithQuestion(id: QuestionId)(
      block: Question => Fu[Result])(implicit ctx: Context): Fu[Result] =
    OptionFuResult(api.question findById id)(block)

  protected def WithQuestion(id: QuestionId, slug: String)(
      block: Question => Fu[Result])(implicit ctx: Context): Fu[Result] =
    WithQuestion(id) { q =>
      if (slug != q.slug)
        fuccess(Redirect {
          controllers.routes.QaQuestion.show(id, q.slug)
        })
      else block(q)
    }

  protected def WithOwnQuestion(id: QuestionId)(block: Question => Fu[Result])(
      implicit ctx: Context): Fu[Result] =
    WithQuestion(id) { q =>
      if (QaAuth canEdit q) block(q)
      else fuccess(Unauthorized)
    }
  protected def WithOwnQuestion(id: QuestionId, slug: String)(
      block: Question => Fu[Result])(implicit ctx: Context): Fu[Result] =
    WithQuestion(id, slug) { q =>
      if (QaAuth canEdit q) block(q)
      else fuccess(Unauthorized)
    }

  protected def WithOwnAnswer(questionId: QuestionId, answerId: AnswerId)(
      block: Question => Answer => Fu[Result])(
      implicit ctx: Context): Fu[Result] =
    api.question findById questionId zip (api.answer findById answerId) flatMap {
      case (Some(q), Some(a)) if (QaAuth canEdit a) => block(q)(a)
      case (Some(q), Some(a)) => fuccess(Unauthorized)
      case _ => notFound
    }

  protected def fetchPopular = api.question popular 20
}
