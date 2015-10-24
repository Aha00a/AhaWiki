package controllers

import javax.inject.{Inject, Singleton}

import actors.ActorPageProcessor
import actors.ActorPageProcessor.Calculate
import akka.actor.ActorSystem
import logics.Cache
import models.{WikiContext, Database, MockDb}
import play.api.cache.CacheApi
import play.api.mvc._
import utils.RequestUtil

import scala.util.Random
import implicits.Implicits._

@Singleton
class Dev @Inject()(implicit cacheApi: CacheApi, system: ActorSystem) extends Controller {
  val actorSimilarPage = system.actorOf(ActorPageProcessor.props)

  def reset = Action { implicit request =>
    val result = Redirect(RequestUtil.refererOrRoot(request))
    if(request.isLocalhost) {
      MockDb.pageFromFile().foreach(p => {
        Database.pageDelete(p.name)
        Database.pageInsert(p.name, p.revision, p.time, p.author, p.remoteAddress, p.content, p.comment.getOrElse(""))
        actorSimilarPage ! Calculate(p.name)
      })

      implicit val wikiContext = WikiContext("")
      Cache.PageList.invalidate()
      Cache.Header.invalidate()
      Cache.Footer.invalidate()
      Cache.Config.invalidate()

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
}




