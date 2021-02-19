package logics

import actors.ActorAhaWiki.Geocode
import akka.actor.ActorRef
import logics.wikis.interpreters.Interpreters
import models.LatLng
import models.WikiContext
import models.tables.Page
import play.api.Logging
import play.api.cache.SyncCacheApi
import play.api.db.Database

import scala.concurrent.duration._

object AhaWikiCache extends Logging {

  trait CacheEntity {
    val key: String = getClass.getName

    def invalidate()(implicit syncCacheApi: SyncCacheApi): Unit = {
      logger.info(s"Invalidate Cache: $key")
      syncCacheApi.remove(key)
    }
  }

  private val durationExpire: FiniteDuration = {
    1.millisecond
//    60.minutes
  }

  // TODO: remove
  object Header extends CacheEntity {
    def get()(implicit wikiContext: WikiContext): String = {
      wikiContext.database.withConnection { implicit connection =>
        import models.tables.Site
        implicit val site: Site = Site.selectWhereDomain(wikiContext.provider.host).getOrElse(Site(-1, ""))
        Interpreters.toHtmlString(Page.selectLastRevision(".header").map(_.content).getOrElse(""))
      }
    }
  }

  // TODO: remove
  object Footer extends CacheEntity {
    def get()(implicit wikiContext: WikiContext): String = {
      wikiContext.database.withConnection { implicit connection =>
        import models.tables.Site
        implicit val site: Site = Site.selectWhereDomain(wikiContext.provider.host).getOrElse(Site(-1, ""))
        Interpreters.toHtmlString(Page.selectLastRevision(".footer").map(_.content).getOrElse(""))
      }
    }
  }

  // TODO: remove
  object Config extends CacheEntity {

    import models.tables.Site
    import play.api.mvc.Request

    def get()(implicit database: Database, site: Site): String = {
      database.withConnection { implicit connection =>
        Page.selectLastRevision(".config").map(_.content).getOrElse("")
      }
    }
  }

}
