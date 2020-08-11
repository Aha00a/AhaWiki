package controllers

import java.time.LocalDateTime
import java.util.Date

import akka.actor.{ActorRef, ActorSystem}
import com.aha00a.commons.Implicits._
import com.aha00a.play.Implicits._
import javax.inject._
import logics.wikis.{PageLogic, WikiPermission}
import models.{PageContent, WikiContext}
import play.api.Configuration
import play.api.cache.SyncCacheApi
import play.api.data.Form
import play.api.data.Forms._
import play.api.mvc._

class Diary @Inject()(implicit val
                      controllerComponents: ControllerComponents,
                      syncCacheApi: SyncCacheApi,
                      actorSystem: ActorSystem,
                      database: play.api.db.Database,
                      @Named("db-actor") actorAhaWiki: ActorRef,
                      configuration: Configuration
                     ) extends BaseController {

  import logics.AhaWikiInjects

  implicit val ahaWikiInjects: AhaWikiInjects = AhaWikiInjects()

  def write(): Action[AnyContent] = Action { implicit request: Request[Any] =>
    val q = Form("q" -> text).bindFromRequest.get
    val now: LocalDateTime = LocalDateTime.now
    val name: String = now.toIsoLocalDateString
    implicit val wikiContext: WikiContext = WikiContext(name)

    database.withConnection { implicit connection =>
      import models.WikiContext.IdProvider
      implicit val idProvider: IdProvider = wikiContext.idProvider

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
