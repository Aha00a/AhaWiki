package controllers

import com.aha00a.commons.Implicits._
import javax.inject._
import logics.ApplicationConf
import logics.wikis.PageLogic
import models.PageContent
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
    import java.net.URLEncoder

    import models.WikiContext.IdProvider

    implicit val idProvider: IdProvider = IdProvider.createBy(request)
    Redirect(routes.Wiki.view(URLEncoder.encode(PageLogic.getListPageByPermission().random().name, "utf-8"), 0, "")).flashing(request.flash)
  }

  def robotsTxt: Action[AnyContent] = Action { implicit request =>
    database.withConnection { implicit connection =>
      Ok(models.tables.Page.selectLastRevision(".robots.txt").map(p => PageContent(p.content).content).getOrElse(""))
    }
  }

  def adsTxt: Action[AnyContent] = Action { implicit request =>
    ApplicationConf().AhaWiki.google.AdSense.adsTxtContent().toOption
      .map(s => Ok(s))
      .getOrElse(NotFound(""))
  }
}
