package models

import akka.actor.ActorRef
import logics.AhaWikiCache
import logics.ApplicationConf
import logics.wikis.RenderingMode
import logics.wikis.RenderingMode.RenderingMode
import models.tables.Site
import play.api.db.Database
import play.api.mvc.Request

import java.time.LocalDate

object ContextWikiPage {
  def apply(name: String)(
    implicit
    request: Request[Any],
    database: Database,
    actorAhaWiki: ActorRef,
    applicationConf: ApplicationConf,
    ahaWikiCache: AhaWikiCache,
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
    ahaWikiCache: AhaWikiCache,
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
  ahaWikiCache: AhaWikiCache,
  requestWrapper: RequestWrapper,
  site: Site,
  val localDateNow: LocalDate = LocalDate.now(),
) extends ContextSite {
  def name: String = seqName.last
  def nameTop: String = seqName.head
  def nameBottom: String = seqName.last
  def push(name: String) = new ContextWikiPage(name +: seqName, renderingMode)

  def at(localDateNow: LocalDate): ContextWikiPage = {
    new ContextWikiPage(seqName, renderingMode)(database, actorAhaWiki, applicationConf, ahaWikiCache, requestWrapper, site, localDateNow)
  }
}
