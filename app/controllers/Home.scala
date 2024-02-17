package controllers

import com.aha00a.commons.Implicits._
import javax.inject._
import logics.ApplicationConf
import logics.wikis.PageLogic
import models.PageContent
import models.tables.Site
import play.api.Configuration
import play.api.cache.SyncCacheApi
import play.api.mvc._

class Home @Inject() (
                       implicit val
                       controllerComponents: ControllerComponents,
                       database:play.api.db.Database,
                       configuration: Configuration
                     ) extends BaseController {
  def index: Action[AnyContent] = Action { implicit request =>
    Redirect(routes.Wiki.view("FrontPage", 0, "")).flashing(request.flash)
  }

  def random: Action[AnyContent] = Action { implicit request =>
    import com.aha00a.commons.utils.UriUtil
    import models.ContextSite.RequestWrapper

    implicit val provider: RequestWrapper = RequestWrapper()
    database.withConnection { implicit connection =>
      implicit val site: Site = Site.selectWhereDomain(request.host).getOrElse(Site(-1, ""))
      val name = PageLogic.getListPageByPermission().random().name
      Redirect(routes.Wiki.view(UriUtil.encodeURIComponent(name), 0, "")).flashing(request.flash)
    }
  }

  def robotsTxt: Action[AnyContent] = Action { implicit request =>
    database.withConnection { implicit connection =>
      implicit val site: Site = Site.selectWhereDomain(request.host).getOrElse(Site(-1, ""))

      Ok(models.tables.Page.selectLastRevision(".robots.txt").map(p => PageContent(p.content).content).getOrElse(
        """User-agent: *
          |Disallow: /
          |
          |User-agent: Googlebot
          |Allow: /
          |
          |User-agent: Mediapartners-Google
          |Allow: /
          |
          |User-agent: DuckDuckBot
          |Allow: /
          |""".stripMargin))
    }
  }

  def adsTxt: Action[AnyContent] = Action { implicit request =>
    ApplicationConf().AhaWiki.google.AdSense.adsTxtContent().toOption
      .map(s => Ok(s))
      .getOrElse(NotFound(""))
  }
}
