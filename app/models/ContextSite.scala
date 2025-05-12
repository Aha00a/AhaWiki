package models

import akka.actor.ActorRef
import logics.ApplicationConf
import logics.wikis.PageLogic
import logics.wikis.RenderingMode.RenderingMode
import models.tables.Page
import models.tables.PageWithoutContentWithSize
import models.tables.Site
import play.api.db.Database
import play.api.mvc.Request

object ContextSite {

  def apply()(
    implicit
    database: Database,
    actorAhaWiki: ActorRef,
    applicationConf: ApplicationConf,
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
  requestWrapper: RequestWrapper,
  val site: Site,
) extends Context {
  //noinspection ScalaWeakerAccess
  lazy val (
    setPageNameAll: Set[String],
    listPageByPermission: List[PageWithoutContentWithSize]
    ) = database.withConnection { implicit connection =>
    (
      Page.selectSeqPageName().toSet,
      PageLogic.getListPageByPermission()
    )
  }
  lazy val seqPageNameByPermission: Seq[String] = listPageByPermission.map(_.name)
  lazy val setPageNameByPermission: Set[String] = seqPageNameByPermission.toSet

  def pageCanSee(name: String): Boolean = !setPageNameAll.contains(name) || setPageNameByPermission.contains(name)

  def toWikiContext(seqName: Seq[String], renderingMode: RenderingMode) = new ContextWikiPage(seqName, renderingMode)
}
