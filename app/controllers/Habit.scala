package controllers

import akka.actor.ActorRef
import akka.actor.ActorSystem
import logics.AhaWikiCache
import logics.ApplicationConf
import logics.SiteLogic
import models.ContextSite
import models.tables.Site
import play.api.Environment
import play.api.Logging
import play.api.db.Database
import play.api.libs.ws.WSClient
import play.api.mvc._

import java.net.URLDecoder
import java.time.LocalDate
import javax.inject._
import scala.concurrent.ExecutionContext

class Habit @Inject()(implicit val
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

  def index: Action[AnyContent] = Action { implicit request =>
    database.withConnection { implicit connection =>
      implicit val site: Site = SiteLogic.get(request.host)
      implicit val contextSite: ContextSite = ContextSite()

      Ok(views.html.Habit.index())
    }
  }

  def view(habitType: String): Action[AnyContent] = Action { implicit request =>
    database.withConnection { implicit connection =>
      implicit val site: Site = SiteLogic.get(request.host)
      implicit val contextSite: ContextSite = ContextSite()

      val decodedHabitType = URLDecoder.decode(habitType.replace("+", "%2B"), "UTF-8").trim
      if (decodedHabitType.isEmpty) {
        BadRequest("habit type is required")
      } else {
        val today = LocalDate.now()
        val dateFrom7d = today.minusDays(6)
        val dateFrom30d = today.minusDays(29)

        val totalCount = models.tables.Habit.countByType(decodedHabitType)
        val count7d = models.tables.Habit.countByTypeSince(decodedHabitType, dateFrom7d)
        val count30d = models.tables.Habit.countByTypeSince(decodedHabitType, dateFrom30d)
        val activeUsers7d = models.tables.Habit.countDistinctUsersByTypeSince(decodedHabitType, dateFrom7d)
        val activeUsers30d = models.tables.Habit.countDistinctUsersByTypeSince(decodedHabitType, dateFrom30d)

        val currentUser = contextSite.requestWrapper.getUser.map(_.seq)
        val myCount30d = currentUser.map(userSeq => models.tables.Habit.countByTypeSinceForUser(decodedHabitType, userSeq, dateFrom30d))
        val myDaily30d = currentUser
          .map(userSeq => models.tables.Habit.selectDailyCountByTypeSinceForUser(decodedHabitType, userSeq, dateFrom30d))
          .getOrElse(Seq.empty)

        Ok(
          views.html.Habit.view(
            decodedHabitType,
            totalCount,
            count7d,
            count30d,
            activeUsers7d,
            activeUsers30d,
            myCount30d,
            myDaily30d,
            dateFrom7d.toString,
            dateFrom30d.toString,
            today.toString,
          ),
        )
      }
    }
  }
}
