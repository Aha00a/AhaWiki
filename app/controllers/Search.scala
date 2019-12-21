package controllers

import java.util.Date

import akka.actor.ActorRef
import com.aha00a.commons.Implicits._
import javax.inject._
import logics.wikis.WikiPermission
import models.{AhaWikiQuery, PageContent, WikiContext}
import play.api.Configuration
import play.api.cache.CacheApi
import play.api.mvc._

class Search @Inject()(implicit
                       cacheApi: CacheApi,
                       database: play.api.db.Database,
                       @Named("db-actor") actorAhaWiki: ActorRef,
                       configuration: Configuration
                      ) extends Controller {
  def index(q: String): Action[AnyContent] = Action { implicit request => database.withConnection { implicit connection =>
    implicit val wikiContext: WikiContext = WikiContext("")

    Ok(views.html.Search.search(
      q,
      q.toOption.map(
        AhaWikiQuery().pageSearch(_)
          .filter(sr => WikiPermission.isReadable(PageContent(sr.content)))
          .sortBy(_.dateTime)(Ordering[Date].reverse)
          .partition(_.name == q)
          .concat()
          .map(_.summarise(q))
      ).getOrElse(Seq.empty)
    ))
  }}
}
