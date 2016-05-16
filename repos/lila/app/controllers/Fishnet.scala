package controllers

import play.api.data.Form
import play.api.libs.json._
import play.api.mvc._
import scala.util.{Try, Success, Failure}

import lila.app._
import lila.common.HTTPRequest
import lila.fishnet.JsonApi.readers._
import lila.fishnet.JsonApi.writers._
import lila.fishnet.{JsonApi, Work}

object Fishnet extends LilaController {

  private def env = Env.fishnet
  private def api = env.api
  override val logger = lila.log("fishnet")

  def clientIp(req: RequestHeader) = lila.fishnet.Client.IpAddress {
    HTTPRequest lastRemoteAddress req
  }

  def acquire = ClientAction[JsonApi.Request.Acquire] { req => client =>
    api acquire client
  }

  def move(workId: String) = ClientAction[JsonApi.Request.PostMove] {
    data => client =>
      api.postMove(Work.Id(workId), client, data) >> api.acquire(client)
  }

  def analysis(workId: String) = ClientAction[JsonApi.Request.PostAnalysis] {
    data => client =>
      api.postAnalysis(Work.Id(workId), client, data) >> api.acquire(client)
  }

  def abort(workId: String) = ClientAction[JsonApi.Request.Acquire] {
    req => client =>
      api.abort(Work.Id(workId), client) inject none
  }

  private def ClientAction[A <: JsonApi.Request](
      f: A => lila.fishnet.Client => Fu[Option[JsonApi.Work]])(
      implicit reads: Reads[A]) =
    Action.async(BodyParsers.parse.tolerantJson) { req =>
      req.body
        .validate[A]
        .fold(
            err => {
              logger.warn(s"Malformed request: $err\n${req.body}")
              BadRequest(jsonError(JsError toJson err)).fuccess
            },
            data =>
              api.authenticateClient(data, clientIp(req)) flatMap {
                case Failure(msg) => {
                    val ip = lila.common.HTTPRequest.lastRemoteAddress(req)
                    logger.info(
                        s"key: ${data.fishnet.apikey} ip: $ip | ${msg.getMessage}")
                    Unauthorized(jsonError(msg.getMessage)).fuccess
                  }
                case Success(client) =>
                  f(data)(client).map {
                    case Some(work) => Accepted(Json toJson work)
                    case _ => NoContent
                  }
            })
    }
}
