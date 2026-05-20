package controllers

import org.apache.pekko.actor.ActorRef
import org.apache.pekko.actor.ActorSystem
import com.aha00a.commons.Implicits._
import logics.AhaWikiCache
import logics.ApplicationConf
import logics.SiteLogic
import logics.wikis.WikiPermission
import models.ContextWikiPage
import models.PageContent
import play.api.Environment
import play.api.Logging
import play.api.db.Database
import play.api.libs.ws.WSClient
import play.api.mvc._

import java.time.LocalDateTime
import javax.inject._
import scala.concurrent.ExecutionContext

class Search @Inject()(implicit val
controllerComponents: ControllerComponents,
                       actorSystem: ActorSystem,
                       database: Database,
                       environment: Environment,
                       @Named("db-actor") actorAhaWiki: ActorRef,
                       applicationConf: ApplicationConf,
                       ahaWikiCache: AhaWikiCache,
                       wsClient: WSClient,
                       executionContext: ExecutionContext
                      ) extends BaseController with Logging {

  def index(q: String): Action[AnyContent] = Action { implicit request => database.withConnection { implicit connection =>

    import models.RequestWrapper
    import models.tables.SearchResultSummary
    import models.tables.Site
    implicit val site: Site = SiteLogic.get(request.host)
    implicit val contextWikiPage: ContextWikiPage = ContextWikiPage("")
    implicit val provider: RequestWrapper = contextWikiPage.requestWrapper

    val wikiPermission = WikiPermission()
    val seq: Seq[SearchResultSummary] = q.toOption.map(q =>
      models.tables.Page.pageSearch(q)
        .filter(sr => {
          val pageContent = PageContent(sr.content)
          wikiPermission.isReadable(sr.name, pageContent)
        })
        .sortBy(_.dateTime)(Ordering[LocalDateTime].reverse)
        .partition(_.name == q)
        .concat()
        .map(_.summarise(q))
    ).getOrElse(Seq.empty)

    Ok(views.html.Search.search(q, seq))
  }}
}
