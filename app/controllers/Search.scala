package controllers

import javax.inject._

import logics.OnApplicationStart
import logics.wikis.WikiPermission
import models.{PageContent, WikiContext, Database}
import models.Database.SearchResult
import play.api.cache.CacheApi
import play.api.mvc._
import com.aha00a.commons.implicits.Implicits._

class Search @Inject() (implicit on:OnApplicationStart, cacheApi: CacheApi) extends Controller {
  def index(q:String) = Action { implicit request =>
    implicit val wikiContext: WikiContext = WikiContext("")
    implicit class RichTuple[T](t:(Iterator[T], Iterator[T])) {
      def concat(): Iterator[T] = t._1 ++ t._2
    }

    Ok(views.html.Search.search(
      q,
      q.toOption.map(
        Database.pageSearch(_)
          .filter(sr => WikiPermission.isReadable(PageContent(sr._2)))
          .map(sr => SearchResult(
            sr._1,
            sr._2.split( """(\r\n|\n)+""").filter(_.contains(q)).mkString(" ... ")
          ))
          .partition(_.name == q)
          .concat()
      ).getOrElse(Iterator.empty)))
  }

}
