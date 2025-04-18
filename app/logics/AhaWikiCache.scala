package logics

import logics.wikis.RenderingMode
import logics.wikis.interpreters.Interpreters
import models.ContextSite
import models.ContextWikiPage
import models.tables.Page
import models.tables.Site
import play.api.Logging
import play.api.cache.SyncCacheApi

import java.sql.Connection
import scala.concurrent.duration._

// TODO: remove
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
    def get()(implicit wikiContext: ContextSite): String = {
      wikiContext.database.withConnection { implicit connection =>
        implicit val context: ContextWikiPage = wikiContext.toWikiContext(Seq(""), RenderingMode.Normal)
        implicit val site: Site = context.site
        Interpreters.toHtmlString(Page.selectLastRevision(".header").map(_.content).orElse(DefaultPageLogic.getOption(".header").toOption).getOrElse(""))
      }
    }
  }

  // TODO: remove
  object Footer extends CacheEntity {
    def get()(implicit wikiContext: ContextSite): String = {
      wikiContext.database.withConnection { implicit connection =>
        implicit val context: ContextWikiPage = wikiContext.toWikiContext(Seq(""), RenderingMode.Normal)
        implicit val site: Site = context.site
        Interpreters.toHtmlString(Page.selectLastRevision(".footer").map(_.content).orElse(DefaultPageLogic.getOption(".footer").toOption).getOrElse(""))
      }
    }
  }

  // TODO: remove
  object Config extends CacheEntity {
    def get()(implicit connection: Connection, site: Site): String = {
      Page.selectLastRevision(".config").map(_.content).getOrElse("")
    }
  }

}
