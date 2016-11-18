package controllers

import javax.inject.{Inject, Singleton}

import actors.ActorPageProcessor
import actors.ActorPageProcessor.Calculate
import akka.actor.ActorSystem
import com.aha00a.commons.implicits.Implicits
import logics.Cache
import logics.wikis.interpreters.InterpreterVim
import models.Database.Page
import models.{WikiContext, Database, MockDb}
import play.api.cache.CacheApi
import play.api.mvc._
import com.aha00a.commons.utils.RequestUtil

import scala.util.Random
import Implicits._

@Singleton
class Dev @Inject()(implicit cacheApi: CacheApi, system: ActorSystem) extends Controller {
  val actorSimilarPage = system.actorOf(ActorPageProcessor.props)

  def reset = Action { implicit request =>
    val result = Redirect(RequestUtil.refererOrRoot(request))
    if(request.isLocalhost) {
      val pageFromFile: Array[Page] = MockDb.pageFromFile()
      pageFromFile.foreach(p => {
        Database.pageDeleteWithRelatedData(p.name)
        Database.pageInsert(p.name, p.revision, p.time, p.author, p.remoteAddress, p.content, p.comment.getOrElse(""))
      })

      implicit val wikiContext = WikiContext("")
      Cache.PageList.invalidate()
      Cache.Header.invalidate()
      Cache.Footer.invalidate()
      Cache.Config.invalidate()

      pageFromFile.foreach(p => {
        actorSimilarPage ! Calculate(p.name)
      })

      result.flashing("success" -> "Reset Succeed.")
    }
    else
    {
      result.flashing("error" -> "Reset Failed")
    }
  }

  def reindex = Action { implicit request =>
    val result = Redirect(RequestUtil.refererOrRoot(request))
    if(request.isLocalhost) {
      Random.shuffle(Database.pageSelectNameGroupByNameOrderByName).foreach(s => actorSimilarPage ! Calculate(s))
      result.flashing("success" -> "Reindex Succeed.")
    }
    else
    {
      result.flashing("error" -> "Reindex Failed")
    }
  }

  def deleteVimCache(md5:String) = Action { implicit request =>
    val result = Redirect(RequestUtil.refererOrRoot(request))

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




