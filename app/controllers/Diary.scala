package controllers

import org.apache.pekko.actor.ActorSystem
import com.aha00a.commons.Implicits._
import com.aha00a.play.Implicits._
import logics.AhaWikiCache
import logics.ApplicationConf
import logics.CrossInstanceBus
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
                      crossInstanceBus: CrossInstanceBus,
                      wsClient: WSClient,
                      executionContext: ExecutionContext
                     ) extends BaseController with FormResults {
  def write(): Action[AnyContent] = Action { implicit request: Request[Any] => withForm(Form("q" -> text)) { q =>
    val now: LocalDateTime = LocalDateTime.now
    val name: String = now.toIsoLocalDateString

    database.withConnection { implicit connection =>
      import models.RequestWrapper
      import models.tables.Site
      implicit val site: Site = SiteLogic.get(request.host)
      implicit val contextWikiPage: ContextWikiPage = ContextWikiPage(name)
      implicit val provider: RequestWrapper = contextWikiPage.requestWrapper
      val latestPage = models.tables.Page.selectLastRevision(name)
      val (latestText: String, latestRevision: Long) = latestPage.map(w => (w.content, w.revision)).getOrElse(("", 0L))
      val permission: WikiPermission = WikiPermission()
      // A missing page is a creation and needs Create, as it does everywhere else. Until
      // 2026-09-12 the empty text was passed wrapped in Some, which reads as an existing page,
      // so Edit alone created today's page.
      if (permission.isWritable(name, latestPage.map(page => PageContent(page.content)))) {
        val body =
          if (latestText == "")
            s"[[DayHeader]]\n * $q"
          else
            s"$latestText\n * $q"

        val nextRevision = latestRevision + 1
        val dateInserted = LocalDateTime.now()
        PageLogic.insert(name, nextRevision, dateInserted, "add item", isMinorEdit = false, body)
        val editorNickname = provider.getUser.map(_.nickname).getOrElse("Guest")
        // Today's page is one anyone may be watching, and this appends a line to it. The writer's
        // own browser is redirected to the page, so there is no watcher of it to skip.
        crossInstanceBus.publishPageUpdated(site.seq, name, nextRevision, editorNickname, dateInserted, None)
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
  }}

}
