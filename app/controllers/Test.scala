package controllers
import anorm.SQL
import anorm.SqlParser.scalar
import com.aha00a.tests.TestUtil
import com.aha00a.tests.UnitTestSuite
import logics.AhaWikiCache
import logics.ApplicationConf
import logics.SiteLogic
import models._
import models.tables.Site
import play.api.{Environment, Mode}
import play.api.Logging
import play.api.db.Database
import play.api.libs.json.Json
import play.api.mvc._

import java.io.File
import javax.inject.Inject

class Test @Inject()(implicit val
                     controllerComponents: ControllerComponents,
                     database: Database,
                     environment: Environment,
                     wikiActors: WikiActors,
                     applicationConf: ApplicationConf,
                     ahaWikiCache: AhaWikiCache
                    ) extends BaseController with Logging {

  private val testUtil = new TestUtil(x => logger.error(x.toString))

  private def devOnly(block: Request[AnyContent] => Result): Action[AnyContent] = Action { implicit request =>
    if (environment.mode == Mode.Dev) block(request) else NotFound
  }

  private def elapsedMillisSince(startedAtNanos: Long): Long =
    (System.nanoTime() - startedAtNanos) / 1000000

  def hc: Action[AnyContent] = Action {
    database.withConnection { implicit connection =>
      SQL("SELECT 1").as(scalar[Int].single)
    }

    val fileAbsolute = new File(".").getAbsoluteFile
    val total = fileAbsolute.getTotalSpace / 1024.0 / 1024
    val free = fileAbsolute.getFreeSpace / 1024.0 / 1024
    val percent = free / total * 100
    val logMessage: String = f"${free}%,.0f MiB / ${total}%,.0f MiB = $percent%.2f%% free"
    if (percent < 5) {
      logger.error(s"Health check failed: low disk space: $logMessage")
      InsufficientStorage("LOW_DISK_SPACE")
    } else {
      Ok("OK")
    }
  }

  def unit: Action[AnyContent] = devOnly { implicit request =>
    implicit val site: Site = SiteLogic.get(request.host)
    implicit val contextWikiPage: ContextWikiPage = ContextWikiPage("UnitTest")

    val startedAtNanos = System.nanoTime()
    val results = UnitTestSuite.runAll(testUtil) { (name, e) =>
      logger.error(s"Unit test failed: $name", e)
    }
    val failures = results.filterNot(_.passed)
    val payload = Json.obj(
      "passed" -> failures.isEmpty,
      "count" -> Json.obj(
        "total" -> results.size,
        "passed" -> (results.size - failures.size),
        "failed" -> failures.size,
      ),
      "durationMs" -> elapsedMillisSince(startedAtNanos),
      "results" -> results.map { result =>
        Json.obj(
          "name" -> result.name,
          "passed" -> result.passed,
          "durationMs" -> result.durationMs,
        ) ++ result.error.map(error => Json.obj("error" -> error)).getOrElse(Json.obj())
      },
    )

    if (failures.isEmpty) Ok(payload).as(JSON) else InternalServerError(payload).as(JSON)
  }


  def gradient: Action[AnyContent] = devOnly { implicit request =>
    import models.tables.Site
    implicit val site: Site = SiteLogic.get(request.host)
    implicit val contextSite: ContextSite = ContextSite()
    Ok(views.html.Test.gradient(""))
  }
}
