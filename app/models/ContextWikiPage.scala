package models

import akka.actor.ActorRef
import logics.ApplicationConf
import logics.wikis.RenderingMode
import logics.wikis.RenderingMode.RenderingMode
import models.ContextSite.RequestWrapper
import models.tables.Site
import play.api.db.Database
import play.api.mvc.Request

object ContextWikiPage {
  def apply(name: String)(
    implicit
    request: Request[Any],
    database: Database,
    actorAhaWiki: ActorRef,
    applicationConf: ApplicationConf,
    site: Site
  ): ContextWikiPage = {
    implicit val provider: RequestWrapper = RequestWrapper()
    new ContextWikiPage(Seq(name), RenderingMode.Normal)
  }

  def preview(name: String)(
    implicit
    request: Request[Any],
    database: Database,
    actorAhaWiki: ActorRef,
    applicationConf: ApplicationConf,
    site: Site
  ): ContextWikiPage = {
    implicit val provider: RequestWrapper = RequestWrapper()
    new ContextWikiPage(Seq(name), RenderingMode.Preview)
  }
}

class ContextWikiPage(val seqName: Seq[String], val renderingMode: RenderingMode)(
  implicit
  database: Database,
  actorAhaWiki: ActorRef,
  applicationConf: ApplicationConf,
  requestWrapper: RequestWrapper,
  site: Site,
) extends ContextSite {
  def name: String = seqName.last
  def nameTop: String = seqName.head
  def nameBottom: String = seqName.last
  def push(name: String) = new ContextWikiPage(name +: seqName, renderingMode)
}
