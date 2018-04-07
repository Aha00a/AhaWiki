package controllers

import javax.inject._

import logics.wikis.WikiPermission
import models.{AhaWikiDatabase, PageContent, WikiContext}
import models.AhaWikiDatabase.SearchResult
import play.api.cache.CacheApi
import play.api.mvc._
import com.aha00a.commons.implicits.Implicits._
import play.api.db.Database

import scala.collection.immutable
import scala.util.matching.Regex

class Search @Inject() (implicit cacheApi: CacheApi, database:play.api.db.Database) extends Controller {
  def index(q:String) = Action { implicit request =>
    implicit val wikiContext: WikiContext = WikiContext("")


    Ok(views.html.Search.search(
      q,
      q.toOption.map(
        AhaWikiDatabase().pageSearch(_)
          .filter(sr => WikiPermission.isReadable(PageContent(sr.content)))
          .map(_.summarised(q))
          .partition(_.name == q)
          .concat()
      ).getOrElse(Seq.empty)))
  }

}
