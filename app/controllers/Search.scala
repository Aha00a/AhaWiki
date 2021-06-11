package controllers

import java.util.Date

import akka.actor.ActorRef
import akka.actor.ActorSystem
import com.aha00a.commons.Implicits._
import javax.inject._
import logics.PermissionLogic
import logics.SessionLogic
import logics.wikis.WikiPermission
import models.PageContent
import models.ContextWikiPage
import models.tables.Permission
import play.api.Configuration
import play.api.Environment
import play.api.Logging
import play.api.cache.SyncCacheApi
import play.api.db.Database
import play.api.libs.ws.WSClient
import play.api.mvc._

import scala.concurrent.ExecutionContext

class Search @Inject()(implicit val
controllerComponents: ControllerComponents,
                       actorSystem: ActorSystem,
                       database: Database,
                       environment: Environment,
                       @Named("db-actor") actorAhaWiki: ActorRef,
                       configuration: Configuration,
                       wsClient: WSClient,
                       executionContext: ExecutionContext
                      ) extends BaseController with Logging {

  def index(q: String): Action[AnyContent] = Action { implicit request => database.withConnection { implicit connection =>

    import models.ContextSite.RequestWrapper
    import models.tables.SearchResultSummary
    import models.tables.Site
    implicit val site: Site = Site.selectWhereDomain(request.host).getOrElse(Site(-1, ""))
    implicit val contextWikiPage: ContextWikiPage = ContextWikiPage("")
    implicit val provider: RequestWrapper = contextWikiPage.requestWrapper

    val wikiPermission = WikiPermission()
    val seq: Seq[SearchResultSummary] = q.toOption.map(q =>
      models.tables.Page.pageSearch(q)
        .filter(sr => {
          val pageContent = PageContent(sr.content)
          wikiPermission.isReadable(pageContent)
        })
        .sortBy(_.dateTime)(Ordering[Date].reverse)
        .partition(_.name == q)
        .concat()
        .map(_.summarise(q))
    ).getOrElse(Seq.empty)

    Ok(views.html.Search.search(q, seq))
  }}
}
