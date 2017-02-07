package lila.blog

import akka.actor._
import com.typesafe.config.Config

import lila.common.PimpedConfig._

final class Env(config: Config,
                scheduler: lila.common.Scheduler,
                messageApi: lila.message.Api) {

  private val PrismicApiUrl = config getString "prismic.api_url"
  private val PrismicCollection = config getString "prismic.collection"
  private val NotifyDelay = config.duration("notify.delay")
  private val NotifySender = config getString "notify.sender"
  private val LastPostCacheTtl = config.duration("last_post_cache.ttl")

  val RssEmail = config getString "rss.email"

  lazy val api =
    new BlogApi(prismicUrl = PrismicApiUrl, collection = PrismicCollection)

  lazy val lastPostCache =
    new LastPostCache(api, LastPostCacheTtl, PrismicCollection)

  private implicit lazy val notifier = new Notifier(
    blogApi = api,
    messageApi = messageApi,
    lastPostCache = lastPostCache,
    lichessUserId = NotifySender)

  {
    import scala.concurrent.duration._

    scheduler.effect(NotifyDelay, "blog: notify check") {
      notifier.apply
    }
    scheduler.once(1 minute) {
      notifier.apply
    }
  }
}

object Env {

  lazy val current: Env =
    "blog".boot(
      new Env(config = lila.common.PlayApp.loadConfig("blog"),
              scheduler = lila.common.PlayApp.scheduler,
              messageApi = lila.message.Env.current.api))
}
