package controllers

import java.time.LocalDateTime
import java.time.format.TextStyle
import java.util.Locale

import actionCompositions.PostAction
import actors.ActorAhaWiki.Calculate
import akka.actor.{ActorRef, ActorSystem}
import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.DateTimeUtil
import com.aha00a.play.Implicits._
import javax.inject._
import logics.wikis.WikiPermission
import logics.{AhaWikiCache, SessionLogic}
import models.{AhaWikiDatabase, PageContent, WikiContext}
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
  def write() = PostAction { implicit request =>
    val q = Form("q" -> text).bindFromRequest.get
    val now: LocalDateTime = LocalDateTime.now
    val name: String = now.toIsoLocalDateString
    val yearDashMonth: String = now.toYearDashMonthString
    val day = now.getDayOfMonth
    val weekdayName = now.getDayOfWeek.getDisplayName(TextStyle.SHORT, Locale.KOREAN)
    implicit val wikiContext: WikiContext = WikiContext(name)

    val (latestText: String, latestRevision: Long) = AhaWikiDatabase().Page.pageSelectLastRevision(name).map(w => (w.content, w.revision)).getOrElse(("", 0L))

    if (WikiPermission.isWritable(PageContent(latestText))) {
      val body =
        if (latestText == "")
          f"= [$yearDashMonth]-$day%02d $weekdayName\n * $q"
        else
          s"$latestText\n * $q"

      AhaWikiDatabase().pageInsert(name, latestRevision + 1, DateTimeUtil.nowEpochNano, SessionLogic.getId(request).getOrElse("anonymous"), request.remoteAddressWithXRealIp, body, "add item")
      actorAhaWiki ! Calculate(name)
      AhaWikiCache.PageList.invalidate()
      Redirect(routes.Wiki.view(name)).flashing("success" -> "saved.")
    } else {
      Redirect(request.refererOrRoot).flashing("error" -> "forbidden.")
    }
  }

}
