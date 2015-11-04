package controllers

import javax.inject._

import logics.OnApplicationStart
import logics.wikis.WikiPermission
import models.{PageContent, WikiContext, Database}
import models.Database.SearchResult
import play.api.cache.CacheApi
import play.api.mvc._

class Search @Inject() (implicit on:OnApplicationStart, cacheApi: CacheApi) extends Controller {
  def index(q:String) = Action { implicit request =>
    implicit val wikiContext: WikiContext = WikiContext("")

    Ok(views.html.Search.search(q, Database.pageSearch(q)
      .filter(sr => WikiPermission.isReadable(new PageContent(sr._2)))
      .map(sr => SearchResult(sr._1, sr._2.split( """(\r\n|\n)+""").filter(_.contains(q)).mkString(" ... "))))
    )
  }

}
