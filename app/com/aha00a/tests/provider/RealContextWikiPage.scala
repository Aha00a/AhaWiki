package com.aha00a.tests.provider

import logics.AhaWikiCache
import logics.ApplicationConf
import logics.wikis.RenderingMode
import models.ContextWikiPage
import models.RequestWrapper
import models.WikiActors
import models.tables.Site
import models.tables.User
import play.api.Application
import play.api.db.Database

import java.util.Locale

trait RealContextWikiPage {
  def createContextWikiPage()(implicit app: Application): ContextWikiPage = {
    implicit val database: Database = app.injector.instanceOf[Database]
    implicit val wikiActors: WikiActors = WikiActors(null, null)
    implicit val applicationConf: ApplicationConf = app.injector.instanceOf[ApplicationConf]
    implicit val cache: AhaWikiCache = app.injector.instanceOf[AhaWikiCache]
    implicit val requestWrapper: RequestWrapper = new RequestWrapper {
      override def getUser: Option[User.SessionUser] =
        Some(User.SessionUser(1L, "aha00a", Some("aha00a@gmail.com")))

      override def locale: Locale = Locale.US

      override def getQueryString(key: String): Option[String] = Some("")

      override val remoteAddress: String = "0.0.0.0.0"

      override def flashGet(key: String): Option[String] = Some("")

      override def host: String = "localhost"

      override def getUserProfileImageUrl: Option[String] = None
    }
    implicit val site: Site = Site(1, "Aha00a", "Aha00a", "")

    new ContextWikiPage(Seq(getClass.getName), RenderingMode.Normal)
  }
}
