package models

import akka.actor.ActorRef
import logics.AhaWikiCache
import logics.ApplicationConf
import logics.wikis.PageLogic
import logics.wikis.RenderingMode.RenderingMode
import models.tables.Page
import models.tables.PageWithoutContentWithSize
import models.tables.Site
import play.api.db.Database
import play.api.mvc.Request

import scala.reflect.classTag

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
  val setPageName: Set[String] = ahaWikiCache.Page.SetPageName.get()((database, site), classTag[Set[String]])

  lazy val listPageByPermission: List[PageWithoutContentWithSize] = database.withConnection { implicit connection =>
    PageLogic.getListPageByPermission()(requestWrapper, connection, this)
  }
  lazy val seqPageNameByPermission: Seq[String] = listPageByPermission.map(_.name)
  lazy val setPageNameByPermission: Set[String] = seqPageNameByPermission.toSet

  def pageCanSee(name: String): Boolean = !setPageName.contains(name) || setPageNameByPermission.contains(name)

  def toWikiContext(seqName: Seq[String], renderingMode: RenderingMode) = new ContextWikiPage(seqName, renderingMode)
}
