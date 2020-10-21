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

  object PageList extends CacheEntity {

    import models.tables.PageWithoutContentWithSize

    def get()(implicit syncCacheApi: SyncCacheApi, database: Database): List[PageWithoutContentWithSize] = syncCacheApi.getOrElseUpdate(key, 60.minutes) {
      logger.info("Cache miss")
      database.withConnection { implicit connection =>
        Page.pageSelectPageList()
      }
    }
  }

  object Header extends CacheEntity {
    def get()(implicit wikiContext: WikiContext): String = wikiContext.syncCacheApi.getOrElseUpdate(key, 60.minutes) {
      logger.info("Cache miss")
      wikiContext.database.withConnection { implicit connection =>
        Interpreters.toHtmlString(Page.selectLastRevision(".header").map(_.content).getOrElse(""))
      }
    }
  }

  object Footer extends CacheEntity {
    def get()(implicit wikiContext: WikiContext): String = wikiContext.syncCacheApi.getOrElseUpdate(key, 60.minutes) {
      logger.info("Cache miss")
      wikiContext.database.withConnection { implicit connection =>
        Interpreters.toHtmlString(Page.selectLastRevision(".footer").map(_.content).getOrElse(""))
      }
    }
  }

  object Config extends CacheEntity {
    def get()(implicit syncCacheApi: SyncCacheApi, database: Database): String = syncCacheApi.getOrElseUpdate(key, 60.minutes) {
      logger.info("Cache miss")
      database.withConnection { implicit connection =>
        Page.selectLastRevision(".config").map(_.content).getOrElse("")
      }
    }
  }



}
