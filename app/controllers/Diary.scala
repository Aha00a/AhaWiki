package controllers

import java.time.LocalDateTime
import java.time.format.TextStyle
import java.util.Locale
import javax.inject._

import actionCompositions.PostAction
import actors.ActorPageProcessor
import actors.ActorPageProcessor.Calculate
import akka.actor.ActorSystem
import com.aha00a.commons.implicits.Implicits._
import com.aha00a.commons.utils.{RequestUtil, DateTimeUtil}
import logics.wikis.WikiPermission
import logics.{Cache, OnApplicationStart, SessionLogic}
import models.{WikiContext, Database, MockDb, PageContent}
import play.api.cache.CacheApi
import play.api.data.Form
import play.api.data.Forms._
import play.api.mvc._

@Singleton
class Diary @Inject()(implicit on: OnApplicationStart, cacheApi: CacheApi, actorSystem: ActorSystem) extends Controller {

  val actorSimilarPage = actorSystem.actorOf(ActorPageProcessor.props)

  def write() = PostAction { implicit request =>
    val q = Form("q" -> text).bindFromRequest.get
    val now: LocalDateTime = LocalDateTime.now
    val name: String = now.toIsoDateString
    val yearDashMonth: String = now.toYearDashMonth
    val day = now.getDayOfMonth
    val weekdayName = now.getDayOfWeek.getDisplayName(TextStyle.SHORT, Locale.KOREAN)
    implicit val wikiContext: WikiContext = WikiContext(name)

    val (latestText: String, latestRevision: Long) = MockDb.selectPageLastRevision(name).map(w => (w.content, w.revision)).getOrElse(("", 0L))

    if (WikiPermission.isWritable(new PageContent(latestText))) {
      val body = if(latestText == "") s"= [$yearDashMonth]-$day $weekdayName\n * " + q else latestText + "\n * " + q
      Database.pageInsert(name, latestRevision + 1, DateTimeUtil.nowEpochNano, SessionLogic.getId(request).getOrElse("anonymous"), request.remoteAddressWithXRealIp, body, "add item")
      actorSimilarPage ! Calculate(name)
      Cache.PageList.invalidate()
      Redirect(routes.Wiki.view(name)).flashing("success" -> "saved.")
    } else {
      Redirect(RequestUtil.referer(request).getOrElse("/")).flashing("error" -> "forbidden.")
    }
  }

}
