package controllers

import javax.inject.{Inject, Singleton}

import actors.ActorPageProcessor
import actors.ActorPageProcessor.Calculate
import akka.actor.ActorSystem
import models.{DirectQuery, MockDb}
import play.api.mvc._
import utils.RequestUtil

import scala.util.Random

@Singleton
class Dev @Inject()(system: ActorSystem) extends Controller {
  val actorSimilarPage = system.actorOf(ActorPageProcessor.props)

  def reset = Action { implicit request =>
    val result = Redirect(RequestUtil.refererOrRoot(request))
    if(request.headers.get("Host").getOrElse("").startsWith("localhost")) {
      MockDb.pageFromFile().foreach(p => {
        DirectQuery.pageDelete(p.name)
        DirectQuery.pageInsert(p.name, p.revision, p.time, p.author, p.remoteAddress, p.content, p.comment.getOrElse(""))
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
    if(request.headers.get("Host").getOrElse("").startsWith("localhost")) {
      Random.shuffle(DirectQuery.pageSelectNameGroupByNameOrderByName).foreach(s => actorSimilarPage ! Calculate(s))
      result.flashing("success" -> "Reindex Succeed.")
    }
    else
    {
      result.flashing("error" -> "Reindex Failed")
    }
  }
}




