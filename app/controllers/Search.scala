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
    import play.api.Mode
    implicit val site: Site = Site.selectWhereDomain(request.host).getOrElse(Site(-1, ""))
    implicit val wikiContext: ContextWikiPage = ContextWikiPage("")
    implicit val provider: RequestWrapper = wikiContext.provider

    val wikiPermission = WikiPermission()
    val id = SessionLogic.getId(request).getOrElse("")
//    val seqPermission = if(environment.mode == Mode.Dev) Permission.select() else Seq() // TODO: 
    val seqPermission = Seq()
    val permissionLogic = new PermissionLogic(seqPermission)

    var permissionDiff = false
    val seq: Seq[SearchResultSummary] = q.toOption.map(
      models.tables.Page.pageSearch(_)
        .filter(sr => {
          val pageContent = PageContent(sr.content)
          val isReadableFromLegacy = wikiPermission.isReadable(pageContent)
          val isWritableFromLagacy = wikiPermission.isWritable(pageContent)

          val readable = permissionLogic.permitted(sr.name, id, Permission.read)
          val editable = permissionLogic.permitted(sr.name, id, Permission.edit)

          if(isReadableFromLegacy != readable) {
            logger.error(s"${sr.name}\treadable\t$isReadableFromLegacy\t$readable")
            permissionDiff = true
          }

          if(isWritableFromLagacy != editable) {
            logger.error(s"${sr.name}\teditable\t$isWritableFromLagacy\t$editable")
            permissionDiff = true
          }

          isReadableFromLegacy
        })
        .sortBy(_.dateTime)(Ordering[Date].reverse)
        .partition(_.name == q)
        .concat()
        .map(_.summarise(q))
    ).getOrElse(Seq.empty)

    if (permissionDiff) {
      logger.error(permissionLogic.toLogString("permission"))
    }

    Ok(views.html.Search.search(q, seq))
  }}
}
