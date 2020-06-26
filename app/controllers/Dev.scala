package controllers

import akka.actor.{ActorRef, ActorSystem}
import com.aha00a.play.Implicits._
import javax.inject.{Inject, Named}
import logics.wikis.interpreters.InterpreterVim
import play.api.cache.SyncCacheApi
import play.api.mvc._

class Dev @Inject()(
                     implicit val
                     controllerComponents: ControllerComponents,
                     syncCacheApi: SyncCacheApi,
                     system: ActorSystem,
                     database:play.api.db.Database,
                     @Named("db-actor") actorAhaWiki: ActorRef
                   ) extends BaseController {
  def deleteVimCache(md5:String): Action[AnyContent] = Action { implicit request =>
    val result = Redirect(request.refererOrRoot)

    if(InterpreterVim.getCacheFileHtml(InterpreterVim.getCacheDir, md5).delete())
    {
      result.flashing("success" -> "Reindex Succeed.")
    }
    else
    {
      result.flashing("error" -> "Reindex Failed")
    }
  }
}




