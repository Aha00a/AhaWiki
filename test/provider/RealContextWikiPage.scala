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

import java.util.Locale

trait RealContextWikiPage {
  def createContextWikiPage()(implicit app: Application): ContextWikiPage = {
    implicit val database: Database = app.injector.instanceOf[Database]
    implicit val actorRef: ActorRef = null
    implicit val applicationConf: ApplicationConf = app.injector.instanceOf[ApplicationConf]
    implicit val cache: AhaWikiCache = app.injector.instanceOf[AhaWikiCache]
    implicit val requestWrapper: RequestWrapper = new RequestWrapper {
      override def getId: Option[String] = Some("aha00a@gmail.com")

      override def locale: Locale = Locale.US

      override def getQueryString(key: String): Option[String] = Some("")

      override val remoteAddress: String = "0.0.0.0.0"

      override def flashGet(key: String): Option[String] = Some("")

      override def host: String = "localhost"
    }
    implicit val site: Site = Site(1, "Aha00a")

    new ContextWikiPage(Seq(getClass.getName), RenderingMode.Normal)
  }
}
