package models

import akka.actor.ActorRef
import logics.AhaWikiCache
import logics.ApplicationConf
import logics.SiteThemeLogic
import logics.wikis.PageLogic
import logics.wikis.RenderingMode.RenderingMode
import models.tables.PageWithoutContentWithSize
import models.tables.Config
import models.tables.Site
import play.api.db.Database
import play.api.mvc.Request

object ContextSite {
  def apply()(
    implicit
    database: Database,
    actorAhaWiki: ActorRef,
    applicationConf: ApplicationConf,
    ahaWikiCache: AhaWikiCache,
    request: Request[Any],
    site: Site,
  ): ContextSite = {
    implicit val provider: RequestWrapper = RequestWrapper()
    new ContextSite()
  }

  def empty()(
    implicit
    database: Database,
    actorAhaWiki: ActorRef,
    applicationConf: ApplicationConf,
    ahaWikiCache: AhaWikiCache,
    site: Site,
  ): ContextSite = {
    implicit val provider: RequestWrapper = RequestWrapper.empty
    new ContextSite()
  }
}

class ContextSite()(
  implicit
  database: Database,
  actorAhaWiki: ActorRef,
  applicationConf: ApplicationConf,
  ahaWikiCache: AhaWikiCache,
  val requestWrapper: RequestWrapper,
  val site: Site,
) extends Context {
  implicit val tupleDatabaseSite: (Database, Site) = (database, site)
  lazy val setPageName: Set[String] = ahaWikiCache.PageMeta.SeqPageName.get().toSet

  lazy val seqPageByPermission: Seq[PageWithoutContentWithSize] = database.withConnection { implicit connection =>
    PageLogic.getListPageByPermission()(requestWrapper, connection, this)
  }

  lazy val seqPageNameByPermission: Seq[String] = seqPageByPermission.map(_.name)
  lazy val setPageNameByPermission: Set[String] = seqPageNameByPermission.toSet

  private def getSiteHexColorConfig(key: String): String = database.withConnection { implicit connection =>
    Config.select(key)
      .flatMap(config => SiteThemeLogic.normalizeHexColor(config.v))
      .getOrElse("")
  }

  lazy val headerBackgroundColor: String = getSiteHexColorConfig(SiteThemeLogic.HeaderBackgroundColorKey)
  lazy val headerForegroundColor: String = getSiteHexColorConfig(SiteThemeLogic.HeaderForegroundColorKey)
  lazy val bodyBackgroundColor: String = getSiteHexColorConfig(SiteThemeLogic.BodyBackgroundColorKey)
  lazy val bodyForegroundColor: String = getSiteHexColorConfig(SiteThemeLogic.BodyForegroundColorKey)
  lazy val footerBackgroundColor: String = getSiteHexColorConfig(SiteThemeLogic.FooterBackgroundColorKey)
  lazy val footerForegroundColor: String = getSiteHexColorConfig(SiteThemeLogic.FooterForegroundColorKey)

  def pageCanSee(name: String): Boolean = !setPageName.contains(name) || setPageNameByPermission.contains(name)

  def toWikiContext(seqName: Seq[String], renderingMode: RenderingMode) = new ContextWikiPage(seqName, renderingMode)
}
