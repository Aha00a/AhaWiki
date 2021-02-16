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
                       syncCacheApi: SyncCacheApi,
                       database:play.api.db.Database,
                       configuration: Configuration
                     ) extends BaseController {
  def index: Action[AnyContent] = Action { implicit request =>
    Redirect(routes.Wiki.view("FrontPage", 0, "")).flashing(request.flash)
  }

  def random: Action[AnyContent] = Action { implicit request =>
    import com.aha00a.commons.utils.UriUtil
    import models.WikiContext.Provider

    implicit val provider: Provider = Provider.createBy(request)
    database.withConnection { implicit connection =>
      implicit val site: Site = Site.selectWhereDomain(request.host).getOrElse(Site(-1, ""))
      val name = PageLogic.getListPageByPermission().random().name
      Redirect(routes.Wiki.view(UriUtil.encodeURIComponent(name), 0, "")).flashing(request.flash)
    }
  }

  def robotsTxt: Action[AnyContent] = Action { implicit request =>
    database.withConnection { implicit connection =>
      implicit val site: Site = Site.selectWhereDomain(request.host).getOrElse(Site(-1, ""))

      Ok(models.tables.Page.selectLastRevision(".robots.txt").map(p => PageContent(p.content).content).getOrElse(""))
    }
  }

  def adsTxt: Action[AnyContent] = Action { implicit request =>
    ApplicationConf().AhaWiki.google.AdSense.adsTxtContent().toOption
      .map(s => Ok(s))
      .getOrElse(NotFound(""))
  }
}
