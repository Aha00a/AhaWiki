package controllers

import java.time.LocalDateTime
import java.time.format.TextStyle
import java.util.{Date, Locale}

import actionCompositions.PostAction
import actors.ActorAhaWiki.Calculate
import akka.actor.{ActorRef, ActorSystem}
import com.aha00a.commons.Implicits._
import com.aha00a.play.Implicits._
import javax.inject._
import logics.wikis.WikiPermission
import logics.{AhaWikiCache, SessionLogic}
import models.{AhaWikiQuery, Page, PageContent, WikiContext}
import play.api.Configuration
import play.api.cache.CacheApi
import play.api.data.Form
import play.api.data.Forms._
import play.api.mvc._

//noinspection TypeAnnotation
@Singleton
class Diary @Inject()(implicit
                      cacheApi: CacheApi,
                      actorSystem: ActorSystem,
                      database: play.api.db.Database,
                      @Named("db-actor") actorAhaWiki: ActorRef,
                      configuration: Configuration
                     ) extends Controller {
  def write() = PostAction { implicit request: Request[Any] =>
    val q = Form("q" -> text).bindFromRequest.get
    val now: LocalDateTime = LocalDateTime.now
    val name: String = now.toIsoLocalDateString
    implicit val wikiContext: WikiContext = WikiContext(name)

    database.withConnection { implicit connection =>
      val (latestText: String, latestRevision: Long) = AhaWikiQuery().Page.selectLastRevision(name).map(w => (w.content, w.revision)).getOrElse(("", 0L))

      val permission: WikiPermission = WikiPermission()
      if (permission.isWritable(PageContent(latestText))) {
        val body =
          if (latestText == "")
            s"[[DayHeader]]\n * $q"
          else
            s"$latestText\n * $q"

        val page = Page(name, latestRevision + 1, new Date(), SessionLogic.getId(request).getOrElse("anonymous"), request.remoteAddressWithXRealIp, "add item", "", body)
        AhaWikiQuery().Page.insert(page)
        actorAhaWiki ! Calculate(name)
        AhaWikiCache.PageList.invalidate()
        Redirect(routes.Wiki.view(name)).flashing("success" -> "saved.")
      } else {
        Redirect(request.refererOrRoot).flashing("error" -> "forbidden.")
      }
    }
  }

}
