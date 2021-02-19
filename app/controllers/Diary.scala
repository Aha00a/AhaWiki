package controllers

import java.time.LocalDateTime
import java.util.Date

import akka.actor.ActorRef
import akka.actor.ActorSystem
import com.aha00a.commons.Implicits._
import com.aha00a.play.Implicits._
import javax.inject._
import logics.wikis.PageLogic
import logics.wikis.WikiPermission
import models.PageContent
import models.ContextWikiPage
import play.api.Configuration
import play.api.Environment
import play.api.cache.SyncCacheApi
import play.api.data.Form
import play.api.data.Forms._
import play.api.db.Database
import play.api.libs.ws.WSClient
import play.api.mvc._

import scala.concurrent.ExecutionContext

class Diary @Inject()(implicit val
                      controllerComponents: ControllerComponents,
                      actorSystem: ActorSystem,
                      database: Database,
                      @Named("db-actor") actorAhaWiki: ActorRef,
                      configuration: Configuration,
                      wsClient: WSClient,
                      executionContext: ExecutionContext
                     ) extends BaseController {

  def write(): Action[AnyContent] = Action { implicit request: Request[Any] =>
    val q = Form("q" -> text).bindFromRequest.get
    val now: LocalDateTime = LocalDateTime.now
    val name: String = now.toIsoLocalDateString

    database.withConnection { implicit connection =>
      import models.ContextWikiPage.Provider
      import models.tables.Site
      implicit val site: Site = Site.selectWhereDomain(request.host).getOrElse(Site(-1, ""))
      implicit val wikiContext: ContextWikiPage = ContextWikiPage(name)
      implicit val provider: Provider = wikiContext.provider
      val (latestText: String, latestRevision: Long) = models.tables.Page.selectLastRevision(name).map(w => (w.content, w.revision)).getOrElse(("", 0L))
      val permission: WikiPermission = WikiPermission()
      if (permission.isWritable(PageContent(latestText))) {
        val body =
          if (latestText == "")
            s"[[DayHeader]]\n * $q"
          else
            s"$latestText\n * $q"

        PageLogic.insert(name, latestRevision + 1, new Date(), "add item", body)
        Redirect(routes.Wiki.view(name)).flashing("success" -> "saved.")
      } else {
        Redirect(request.refererOrRoot).flashing("error" -> "forbidden.")
      }
    }
  }

}
