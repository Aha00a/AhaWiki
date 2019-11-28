package controllers

import javax.inject._

import com.aha00a.commons.implicits.Implicits._
import logics.wikis.WikiPermission
import models.{AhaWikiDatabase, PageContent, WikiContext}
import play.api.cache.CacheApi
import play.api.mvc._

class Search @Inject() (implicit cacheApi: CacheApi, database:play.api.db.Database) extends Controller {
  def index(q:String) = Action { implicit request =>
    implicit val wikiContext: WikiContext = WikiContext("")

    Ok(views.html.Search.search(
      q,
      q.toOption.map(
        AhaWikiDatabase().pageSearch(_)
          .filter(sr => WikiPermission.isReadable(PageContent(sr.content)))
          .sortBy(-_.time)
          .partition(_.name == q)
          .concat()
          .map(_.summarise(q))
      ).getOrElse(Seq.empty)
    ))
  }

}
