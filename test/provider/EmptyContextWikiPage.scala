package provider

import akka.actor.ActorRef
import logics.AhaWikiCache
import logics.ApplicationConf
import logics.wikis.RenderingMode
import models.ContextWikiPage
import models.RequestWrapper
import models.tables.Site
import play.api.Application
import play.api.db.Database

import java.time.LocalDate

trait EmptyContextWikiPage {
  implicit val application: Application = null
  implicit val database: Database = null
  implicit val actorRef: ActorRef = null
  implicit val applicationConf: ApplicationConf = null
  implicit val cache: AhaWikiCache = null
  implicit val requestWrapper: RequestWrapper = RequestWrapper.empty
  implicit val site: Site = Site.notFound
  implicit val contextWikiPage: ContextWikiPage = new ContextWikiPage(Seq(getClass.getName), RenderingMode.Normal)

  def createContextWikiPage(implicit now: LocalDate): ContextWikiPage = {
    contextWikiPage.at(now)
  }
}
