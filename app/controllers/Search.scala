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

class Search @Inject() (implicit cacheApi: CacheApi, database:play.api.db.Database) extends Controller {
  def index(q:String) = Action { implicit request =>
    implicit val wikiContext: WikiContext = WikiContext("")

    def around(i:Int, distance: Int = 2): immutable.Seq[Int] = (i - distance) to (i + distance)

    val regex = s"(?i)$q".r
    Ok(views.html.Search.search(
      q,
      q.toOption.map(
        AhaWikiDatabase().pageSearch(_)
          .filter(sr => WikiPermission.isReadable(PageContent(sr.content)))
          .map(sr => {
            val lines = sr.content.split("""(\r\n|\n)+""")
            SearchResult(
              sr.name,
              lines
                .zipWithIndex
                .filter(s => regex.findFirstIn(s._1).isDefined)
                .flatMap(s => around(s._2))
                .distinct
                .filter(lines.isDefinedAt(_))
                .toSeq
                .splitBy((a, b) => a + 1 != b)
                .map(_.map(i => i + ": " + lines(i)).mkString("\n"))
                .mkString("\n\n⋯\n\n")
            )
          })
          .partition(_.name == q)
          .concat()
      ).getOrElse(Seq.empty)))
  }

}
