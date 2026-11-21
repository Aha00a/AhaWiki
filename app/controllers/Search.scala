package controllers

import org.apache.pekko.actor.ActorSystem
import com.aha00a.commons.Implicits._
import logics.AhaWikiCache
import logics.ApplicationConf
import logics.SiteLogic
import logics.wikis.WikiPermission
import models.ContextWikiPage
import models.WikiActors
import play.api.Environment
import play.api.Logging
import play.api.db.Database
import play.api.libs.ws.WSClient
import play.api.mvc._

import javax.inject._
import scala.concurrent.ExecutionContext

class Search @Inject()(implicit val
controllerComponents: ControllerComponents,
                       actorSystem: ActorSystem,
                       database: Database,
                       environment: Environment,
                        wikiActors: WikiActors,
                       applicationConf: ApplicationConf,
                       ahaWikiCache: AhaWikiCache,
                       wsClient: WSClient,
                       executionContext: ExecutionContext
                       ) extends BaseController with Logging {
  def index(q: String): Action[AnyContent] = Action { implicit request => database.withConnection { implicit connection =>

    import models.RequestWrapper
    import models.tables.SearchResult
    import models.tables.SearchResultSummary
    import models.tables.Site
    implicit val site: Site = SiteLogic.get(request.host)
    implicit val contextWikiPage: ContextWikiPage = ContextWikiPage("").holding(connection)
    implicit val provider: RequestWrapper = contextWikiPage.requestWrapper

    val wikiPermission = WikiPermission()
    val (readable, moreThanShown) = q.toOption
      .map(q => logics.wikis.ReadablePages.search(models.tables.Page.pageSearch(q, _), SearchResult.MaxPages, wikiPermission.isReadable))
      .getOrElse((Seq.empty, false))
    val seq: Seq[SearchResultSummary] = q.toOption.map(q =>
      readable
        .partition(_.name == q)
        .concat()
        .map(_.summarise(q))
    ).getOrElse(Seq.empty)

    Ok(views.html.Search.search(q, seq, moreThanShown)).withHeaders("X-Robots-Tag" -> "noindex, follow")
  }}
}
