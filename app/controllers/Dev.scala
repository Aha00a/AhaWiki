package controllers

import org.apache.pekko.actor.ActorSystem
import com.aha00a.play.Implicits._
import javax.inject.Inject
import logics.AdminLogic
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

  // The Vim render cache is shared by every site, so only the global admin clears an entry, and
  // only by its md5. Until 2026-09-15 anyone could, and the md5 went into a file path as it came,
  // so md5=../.. deleted any .html file the service could write.
  def deleteVimCache(md5: String): Action[AnyContent] = Action { implicit request =>
    if (!AdminLogic.isAdmin(request)) {
      Forbidden("Access denied.")
    } else if (!md5.matches("[0-9a-f]{32}")) {
      BadRequest("md5 must be 32 lowercase hex digits.")
    } else {
      Redirect(request.refererOrRoot).flashing(
        if (InterpreterVim.getCacheFileHtml(InterpreterVim.getCacheDir, md5).delete()) "success" -> "Reindex Succeed."
        else "error" -> "Reindex Failed"
      )
    }
  }

  def gradient: Action[AnyContent] = devOnly { implicit request =>
    implicit val site: Site = SiteLogic.get(request.host)
    implicit val contextSite: ContextSite = ContextSite()
    Ok(views.html.Test.gradient(""))
  }
}

