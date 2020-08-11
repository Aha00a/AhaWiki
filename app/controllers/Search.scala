package controllers

import java.util.Date

import akka.actor.ActorRef
import com.aha00a.commons.Implicits._
import javax.inject._
import logics.wikis.WikiPermission
import models.PageContent
import models.WikiContext
import play.api.Configuration
import play.api.cache.SyncCacheApi
import play.api.mvc._

class Search @Inject()(implicit val
                       controllerComponents: ControllerComponents,
                       syncCacheApi: SyncCacheApi,
                       database: play.api.db.Database,
                       @Named("db-actor") actorAhaWiki: ActorRef,
                       configuration: Configuration
                      ) extends BaseController {

  import logics.AhaWikiInjects

  implicit val ahaWikiInjects: AhaWikiInjects = AhaWikiInjects()

  def index(q: String): Action[AnyContent] = Action { implicit request => database.withConnection { implicit connection =>
    import logics.IdProvider

    implicit val wikiContext: WikiContext = WikiContext("")
    implicit val idProvider: IdProvider = wikiContext.idProvider

    Ok(views.html.Search.search(
      q,
      q.toOption.map(
        models.tables.Page.pageSearch(_)
          .filter(sr => WikiPermission().isReadable(PageContent(sr.content)))
          .sortBy(_.dateTime)(Ordering[Date].reverse)
          .partition(_.name == q)
          .concat()
          .map(_.summarise(q))
      ).getOrElse(Seq.empty)
    ))
  }}
}
