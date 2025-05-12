package logics

import com.aha00a.commons.utils.StopWatch
import logics.wikis.RenderingMode
import logics.wikis.interpreters.Interpreters
import models.ContextSite
import models.ContextWikiPage
import models.tables.PageWithoutContentWithSize
import models.tables.Site
import models.tables.SiteDomain
import play.api.Environment
import play.api.Logging
import play.api.Mode.Dev
import play.api.cache.SyncCacheApi
import play.api.db.Database

import javax.inject.Inject
import javax.inject.Singleton
import scala.annotation.unused
import scala.concurrent.duration._
import scala.reflect.ClassTag

@Singleton
class AhaWikiCache @Inject()(syncCacheApi: SyncCacheApi, environment: Environment) extends Logging {
  trait CacheEntity[T, I] {
    //noinspection ScalaWeakerAccess
    val durationExpire: FiniteDuration = if (environment.mode == Dev) 5.minute else 1.hour

    def key()(implicit i: I): String
    def keyDefault()(implicit @unused i: I): String = s"${getClass.getName}"

    def invalidate()(implicit i: I): Unit = {
      StopWatch(Seq("Cache", "Invalidate", key()).mkString("\t")) {
        syncCacheApi.remove(key())
      }
    }
    def get()(implicit i: I, @unused classTag: ClassTag[T]): T = syncCacheApi.getOrElseUpdate(key(), durationExpire)(wrapOrElse)

    private def wrapOrElse()(implicit i: I): T = {
      StopWatch(Seq("Cache", "Miss", key()).mkString("\t")) {
        val result = orElse()
        logger.info(s"Cache\tFill\t${key()} = ${result match {
          case seq: Seq[_] => s"Seq[${seq.take(100).grouped(10).map(_.mkString(",")).mkString(",\n")}].size == ${seq.size}"
          case set: Set[_] => s"Set[${set.take(100).grouped(10).map(_.mkString(",")).mkString(",\n")}].size == ${set.size}"
          case map: Map[_, _] => s"Map[${map.take(100).grouped(10).map(_.mkString(",")).mkString(",\n")}].size == ${map.size}"
          case s: String => s"${s.replaceAll("\n", "|||").take(200)}... size == ${s.size}"
          case _ => s"${result.toString.replaceAll("\n", "|||").take(200)}... size == ${result.toString.length}"
        }}")
        result
      }
    }

    def orElse()(implicit i: I): T
  }

  trait CacheEntityWithDatabase[T] extends CacheEntity[T, Database] {
    def orElse()(implicit database: Database): T
  }

  trait CacheEntityWithContextSite[T] extends CacheEntity[T, ContextSite] {
    override def key()(implicit contextSite: ContextSite): String = s"${getClass.getSimpleName}:${contextSite.site}"
  }


  object SiteDomain extends CacheEntityWithDatabase[Seq[SiteDomain]] {
    override def key()(implicit i: Database): String = keyDefault
    override def orElse()(implicit database: Database): Seq[SiteDomain] = database.withConnection { implicit connection =>
      models.tables.SiteDomain.select()
    }

    object Map extends CacheEntityWithDatabase[Map[String, SiteDomain]] {
      override def key()(implicit i: Database): String = keyDefault
      override def orElse()(implicit database: Database): Map[String, SiteDomain] = {
        SiteDomain.get().map(sd => (sd.domain, sd)).toMap
      }
    }
  }

  object Site extends CacheEntityWithDatabase[Seq[Site]] {
    override def key()(implicit i: Database): String = keyDefault
    override def orElse()(implicit database: Database): Seq[Site] = database.withConnection { implicit connection =>
      models.tables.Site.select()
    }
    object Map extends CacheEntityWithDatabase[Map[Long, Site]] {
      override def key()(implicit i: Database): String = keyDefault
      override def orElse()(implicit database: Database): Map[Long, Site] = {
        Site.get().map(sd => (sd.seq, sd)).toMap
      }
    }
  }



  object Header extends CacheEntityWithContextSite[String] {
    override def orElse()(implicit contextSite: ContextSite): String = contextSite.database.withConnection { implicit connection =>
      implicit val context: ContextWikiPage = contextSite.toWikiContext(Seq(""), RenderingMode.Normal)
      implicit val site: Site = context.site
      Interpreters.toHtmlString(models.tables.Page.selectLastRevision(".header").map(_.content).orElse(DefaultPageLogic.getOption(".header").toOption).getOrElse(""))
    }
  }

  object Footer extends CacheEntityWithContextSite[String] {
    override def orElse()(implicit contextSite: ContextSite): String = contextSite.database.withConnection { implicit connection =>
      implicit val context: ContextWikiPage = contextSite.toWikiContext(Seq(""), RenderingMode.Normal)
      implicit val site: Site = context.site
      Interpreters.toHtmlString(models.tables.Page.selectLastRevision(".footer").map(_.content).orElse(DefaultPageLogic.getOption(".footer").toOption).getOrElse(""))
    }
  }

  object Config extends CacheEntityWithContextSite[String] {
    override def orElse()(implicit contextSite: ContextSite): String = contextSite.database.withConnection { implicit connection =>
      implicit val site: Site = contextSite.site
      models.tables.Page.selectLastRevision(".config").map(_.content).getOrElse("")
    }
  }

  object Page {
    object SeqPageWithoutContentWithSizeLatest extends CacheEntity[Seq[PageWithoutContentWithSize], (Database, Site)] {
      override def key()(implicit t2: (Database, Site)): String = s"${getClass.getName}:${t2._2}"
      override def orElse()(implicit t2: (Database, Site)): Seq[PageWithoutContentWithSize] = t2._1.withConnection { implicit connection =>
        implicit val (_, site: Site) = t2
        models.tables.Page.selectSeqPageWithoutContentWithSizeLatest()
      }
    }
  }
}
