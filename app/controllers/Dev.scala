package controllers

import javax.inject.{Inject, Named, Singleton}
import actors.ActorAhaWiki
import actors.ActorAhaWiki.Calculate
import akka.actor.{ActorRef, ActorSystem}
import com.aha00a.commons.implicits.Implicits._
import logics.Cache
import logics.wikis.interpreters.InterpreterVim
import models.AhaWikiDatabase.Page
import models.{AhaWikiDatabase, MockDb, WikiContext}
import play.api.cache.CacheApi
import play.api.mvc._

import scala.util.Random

@Singleton
class Dev @Inject()(
  implicit cacheApi: CacheApi,
  system: ActorSystem,
  database:play.api.db.Database,
  @Named("db-actor") actorAhaWiki: ActorRef
) extends Controller {
  def reset = Action { implicit request =>
    val result = Redirect(request.refererOrRoot)
    if(request.isLocalhost) {
      val pageFromFile: Array[Page] = MockDb().pageFromFile()
      pageFromFile.foreach(p => {
        AhaWikiDatabase().pageDeleteWithRelatedData(p.name)
        AhaWikiDatabase().pageInsert(p.name, p.revision, p.time, p.author, p.remoteAddress, p.content, p.comment.getOrElse(""))
      })

      implicit val wikiContext = WikiContext("")
      Cache.PageList.invalidate()
      Cache.Header.invalidate()
      Cache.Footer.invalidate()
      Cache.Config.invalidate()

      pageFromFile.foreach(p => {
        actorAhaWiki ! Calculate(p.name)
      })

      result.flashing("success" -> "Reset Succeed.")
    }
    else
    {
      result.flashing("error" -> "Reset Failed")
    }
  }

  def reindex = Action { implicit request =>
    val result = Redirect(request.refererOrRoot)
    if(request.isLocalhost) {
      val listPageName: List[String] = Random.shuffle(AhaWikiDatabase().pageSelectNameGroupByNameOrderByName)
      for((v, i) <- listPageName.zipWithIndex) {
        actorAhaWiki ! Calculate(v, i, listPageName.length)
      }
      result.flashing("success" -> "Reindex Succeed.")
    }
    else
    {
      result.flashing("error" -> "Reindex Failed")
    }
  }

  def deleteVimCache(md5:String) = Action { implicit request =>
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




