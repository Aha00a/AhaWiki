package controllers

import akka.actor.{ActorRef, ActorSystem}
import com.aha00a.play.Implicits._
import javax.inject.Inject
import logics.wikis.interpreters.InterpreterVim
import play.api.cache.SyncCacheApi
import play.api.mvc._

class Dev @Inject()(
                     implicit val
                     controllerComponents: ControllerComponents,
                     system: ActorSystem,
                     database:play.api.db.Database,
                     @javax.inject.Named("db-actor") actorAhaWiki: ActorRef
                   ) extends BaseController {
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
}




