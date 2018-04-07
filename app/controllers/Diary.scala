package controllers

import java.time.LocalDateTime
import java.time.format.TextStyle
import java.util.Locale
import javax.inject._

import actionCompositions.PostAction
import actors.ActorPageProcessor
import actors.ActorPageProcessor.Calculate
import akka.actor.{ActorRef, ActorSystem}
import com.aha00a.commons.implicits.Implicits._
import com.aha00a.commons.utils.DateTimeUtil
import logics.wikis.WikiPermission
import logics.{Cache, SessionLogic}
import models.{AhaWikiDatabase, MockDb, PageContent, WikiContext}
import play.api.cache.CacheApi
import play.api.data.Form
import play.api.data.Forms._
import play.api.db.Database
import play.api.mvc._

@Singleton
class Diary @Inject()(implicit cacheApi: CacheApi, actorSystem: ActorSystem, database:play.api.db.Database) extends Controller {

  val actorSimilarPage: ActorRef = actorSystem.actorOf(ActorPageProcessor.props)

  def write() = PostAction { implicit request =>
    val q = Form("q" -> text).bindFromRequest.get
    val now: LocalDateTime = LocalDateTime.now
    val name: String = now.toIsoLocalDateString
    val yearDashMonth: String = now.toYearDashMonthString
    val day = now.getDayOfMonth
    val weekdayName = now.getDayOfWeek.getDisplayName(TextStyle.SHORT, Locale.KOREAN)
    implicit val wikiContext: WikiContext = WikiContext(name)

    val (latestText: String, latestRevision: Long) = MockDb().selectPageLastRevision(name).map(w => (w.content, w.revision)).getOrElse(("", 0L))

    if (WikiPermission.isWritable(PageContent(latestText))) {
      val body = if(latestText == "") f"= [$yearDashMonth]-$day%02d $weekdayName\n * " + q else latestText + "\n * " + q
      AhaWikiDatabase().pageInsert(name, latestRevision + 1, DateTimeUtil.nowEpochNano, SessionLogic.getId(request).getOrElse("anonymous"), request.remoteAddressWithXRealIp, body, "add item")
      actorSimilarPage ! Calculate(name)
      Cache.PageList.invalidate()
      Redirect(routes.Wiki.view(name)).flashing("success" -> "saved.")
    } else {
      Redirect(request.refererOrRoot).flashing("error" -> "forbidden.")
    }
  }

}
