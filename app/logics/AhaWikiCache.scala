package logics

import com.aha00a.commons.utils.StopWatch
import logics.wikis.RenderingMode
import logics.wikis.interpreters.Interpreters
import models.ContextSite
import models.ContextWikiPage
import models.tables.Page
import models.tables.Site
import play.api.Environment
import play.api.Logging
import play.api.Mode.Dev
import play.api.cache.SyncCacheApi

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.duration._

@Singleton
class AhaWikiCache @Inject()(syncCacheApi: SyncCacheApi, environment: Environment) extends Logging {
  trait CacheEntity {
    def key()(implicit contextSite:ContextSite): String = s"${getClass.getSimpleName}:${contextSite.site}"

    def invalidate()(implicit contextSite:ContextSite): Unit = {
      StopWatch(Seq("Cache", "Miss", key()).mkString("\t")) {
        syncCacheApi.remove(key())
      }
    }

    def get()(implicit contextSite: ContextSite): String = syncCacheApi.getOrElseUpdate(key, durationExpire)(wrapOrElse)

    private def wrapOrElse()(implicit contextSite: ContextSite): String = {
      StopWatch(Seq("Cache", "Miss", key()).mkString("\t")) {
        orElse()
      }
    }
    def orElse()(implicit contextSite: ContextSite): String
  }


  private val durationExpire: FiniteDuration = if (environment.mode == Dev) 5.minute else 1.hour

  object Header extends CacheEntity {
    override def orElse()(implicit contextSite: ContextSite): String = contextSite.database.withConnection { implicit connection =>
      implicit val context: ContextWikiPage = contextSite.toWikiContext(Seq(""), RenderingMode.Normal)
      implicit val site: Site = context.site
      Interpreters.toHtmlString(Page.selectLastRevision(".header").map(_.content).orElse(DefaultPageLogic.getOption(".header").toOption).getOrElse(""))
    }
  }

  object Footer extends CacheEntity {
    override def orElse()(implicit contextSite: ContextSite): String = contextSite.database.withConnection { implicit connection =>
      implicit val context: ContextWikiPage = contextSite.toWikiContext(Seq(""), RenderingMode.Normal)
      implicit val site: Site = context.site
      Interpreters.toHtmlString(Page.selectLastRevision(".footer").map(_.content).orElse(DefaultPageLogic.getOption(".footer").toOption).getOrElse(""))
    }
  }

  object Config extends CacheEntity {
    override def orElse()(implicit contextSite: ContextSite): String = contextSite.database.withConnection { implicit connection =>
      implicit val site: Site = contextSite.site
      Page.selectLastRevision(".config").map(_.content).getOrElse("")
    }
  }
}
