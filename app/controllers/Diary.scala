package controllers

import org.apache.pekko.actor.ActorSystem
import com.aha00a.commons.Implicits._
import com.aha00a.play.Implicits._
import logics.AhaWikiCache
import logics.ApplicationConf
import logics.SiteLogic
import logics.TelegramLogic
import logics.wikis.PageLogic
import logics.wikis.WikiPermission
import models.ContextWikiPage
import models.PageContent
import models.WikiActors
import play.api.data.Form
import play.api.data.Forms._
import play.api.db.Database
import play.api.libs.ws.WSClient
import play.api.mvc._

import java.time.LocalDateTime
import javax.inject._
import scala.concurrent.ExecutionContext

class Diary @Inject()(implicit val
                      controllerComponents: ControllerComponents,
                      actorSystem: ActorSystem,
                      database: Database,
                       wikiActors: WikiActors,
                      applicationConf: ApplicationConf,
                      ahaWikiCache: AhaWikiCache,
                      telegramLogic: TelegramLogic,
                      wsClient: WSClient,
                      executionContext: ExecutionContext
                     ) extends BaseController {
  def write(): Action[AnyContent] = Action { implicit request: Request[Any] =>
    val q = Form("q" -> text).bindFromRequest().get
    val now: LocalDateTime = LocalDateTime.now
    val name: String = now.toIsoLocalDateString

    database.withConnection { implicit connection =>
      import models.RequestWrapper
      import models.tables.Site
      implicit val site: Site = SiteLogic.get(request.host)
      implicit val contextWikiPage: ContextWikiPage = ContextWikiPage(name)
      implicit val provider: RequestWrapper = contextWikiPage.requestWrapper
      val (latestText: String, latestRevision: Long) = models.tables.Page.selectLastRevision(name).map(w => (w.content, w.revision)).getOrElse(("", 0L))
      val permission: WikiPermission = WikiPermission()
      if (permission.isWritable(name, PageContent(latestText))) {
        val body =
          if (latestText == "")
            s"[[DayHeader]]\n * $q"
          else
            s"$latestText\n * $q"

        val nextRevision = latestRevision + 1
        PageLogic.insert(name, nextRevision, LocalDateTime.now(), "add item", isMinorEdit = false, body)
        val editorNickname = provider.getUser.map(_.nickname).getOrElse("Guest")
        val siteChatId = models.tables.Config.Query.Telegram.chatId()
        if (latestText.isEmpty)
          telegramLogic.notifyPageCreated(request.host, name, editorNickname, "add item", siteChatId)
        else
          telegramLogic.notifyPageEdited(request.host, name, nextRevision, editorNickname, "add item", siteChatId)
        implicit val tupleDatabaseSite: (Database, Site) = (database, site)
        ahaWikiCache.PageMeta.SeqPageLatestSummary.invalidate()
        Redirect(routes.Wiki.view(name)).flashing("success" -> "saved.")
      } else {
        Redirect(request.refererOrRoot).flashing("error" -> "forbidden.")
      }
    }
  }

}
