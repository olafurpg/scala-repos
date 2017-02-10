package lila.api

import org.joda.time.DateTime
import play.api.http.HeaderNames
import play.api.mvc.RequestHeader

object Mobile {

  object Api {

    case class Old(version: Int,
                   // date when a newer version was released
                   deprecatedAt: DateTime,
                   // date when the server stops accepting requests
                   unsupportedAt: DateTime)

    def currentVersion = 1

    def oldVersions: List[Old] = List(
        // old version 0 is just an example, so the list is never empty :)
        // nobody ever used version 0.
        Old(version = 0,
            deprecatedAt = new DateTime("2014-08-01"),
            unsupportedAt = new DateTime("2014-12-01")),
    )

    private val PathPattern = """^.+/socket/v(\d+)$""".r

    def requestVersion(req: RequestHeader): Option[Int] = {
      val accepts = ~req.headers.get(HeaderNames.ACCEPT)
      if (accepts contains "application/vnd.lichess.v1+json") some(1)
      else
        req.path match {
          case PathPattern(version) => parseIntOption(version)
          case _ => None
        }
    }

    def requested(req: RequestHeader) = requestVersion(req).isDefined
  }

  object App {

    val currentVersion =
      lila.common.PlayApp.loadConfig getString "mobile.app.version"
  }
}
