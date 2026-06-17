package controllers

import org.apache.pekko.actor.ActorSystem
import com.aha00a.play.Implicits._
import javax.inject.Inject
import logics.AhaWikiCache
import logics.ApplicationConf
import logics.SiteLogic
import logics.wikis.interpreters.InterpreterVim
import models.ContextSite
import models.WikiActors
import models.tables.Site
import play.api.Environment
import play.api.Mode
import play.api.mvc._

class Dev @Inject()(
                      implicit val
                       controllerComponents: ControllerComponents,
                       system: ActorSystem,
                       database: play.api.db.Database,
                       environment: Environment,
                       wikiActors: WikiActors,
                       applicationConf: ApplicationConf,
                       ahaWikiCache: AhaWikiCache,
                     ) extends BaseController {

  private def devOnly(block: Request[AnyContent] => Result): Action[AnyContent] = Action { implicit request =>
    if (environment.mode == Mode.Dev) block(request) else NotFound
  }

  def deleteVimCache(md5:String): Action[AnyContent] = Action { implicit request =>
    Redirect(request.refererOrRoot).flashing(
      if(InterpreterVim.getCacheFileHtml(InterpreterVim.getCacheDir, md5).delete())
      {
        "success" -> "Reindex Succeed."
      }
      else
      {
        "error" -> "Reindex Failed"
      }
    )
  }

  def gradient: Action[AnyContent] = devOnly { implicit request =>
    implicit val site: Site = SiteLogic.get(request.host)
    implicit val contextSite: ContextSite = ContextSite()
    Ok(views.html.Test.gradient(""))
  }
}

