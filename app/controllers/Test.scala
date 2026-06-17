package controllers
import anorm.SQL
import anorm.SqlParser.scalar
import com.aha00a.tests.TestUtil
import com.aha00a.tests.unit.{BlameUnit, CrawlerUnit, HeadingNumberUnit, InterpreterBlockUnit, InterpreterMarkdownUnit, InterpreterSchemaUnit, InterpreterVimUnit, InterpreterWikiUnit, JsonUnit, MacroPeriodUnit, PageContentUnit, PermissionLogicUnit, PermissionUnit, SchemaOrgUnit, SignedReadUrlLogicUnit, TraitInterpreterUnit, UrlDetectorUnit, WikiMacrosUnit, WikiPermissionUnit}
import logics.AhaWikiCache
import logics.ApplicationConf
import logics.SiteLogic
import models._
import models.tables.Site
import play.api.{Environment, Mode}
import play.api.Logging
import play.api.db.Database
import play.api.mvc._

import java.io.File
import javax.inject.Inject
import scala.util.control.NonFatal

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

  def hc: Action[AnyContent] = Action {
    database.withConnection { implicit connection =>
      SQL("SELECT 1").as(scalar[Int].single)
    }

    val fileAbsolute = new File(".").getAbsoluteFile
    val total = fileAbsolute.getTotalSpace / 1024.0 / 1024
    val free = fileAbsolute.getFreeSpace / 1024.0 / 1024
    val percent = free / total * 100
    val message: String = f"${free}%,.0f MiB / ${total}%,.0f MiB = $percent%.2f%% free"
    if (percent < 5) InsufficientStorage(message) else Ok(message)
  }

  def unit: Action[AnyContent] = devOnly { implicit request =>
    implicit val site: Site = SiteLogic.get(request.host)
    implicit val contextWikiPage: ContextWikiPage = ContextWikiPage("UnitTest")

    val unitTests: Seq[(String, () => Unit)] = Seq(
      "InterpreterBlockUnit" -> (() => InterpreterBlockUnit.run(testUtil)),
      "HeadingNumberUnit" -> (() => HeadingNumberUnit.run(testUtil)),
      "InterpreterVimUnit" -> (() => InterpreterVimUnit.run(testUtil)),
      "WikiMacrosUnit" -> (() => WikiMacrosUnit.run(testUtil)),
      "MacroPeriodUnit" -> (() => MacroPeriodUnit.run(testUtil)),
      "UrlDetectorUnit" -> (() => UrlDetectorUnit.run(testUtil)),
      "SchemaOrgUnit" -> (() => SchemaOrgUnit.run(testUtil)),
      "TraitInterpreterUnit" -> (() => TraitInterpreterUnit.run(testUtil)),
      "SignedReadUrlLogicUnit" -> (() => SignedReadUrlLogicUnit.run(testUtil)),
      "InterpreterMarkdownUnit" -> (() => InterpreterMarkdownUnit.run(testUtil)),
      "JsonUnit" -> (() => JsonUnit.run(testUtil)),
      "InterpreterSchemaUnit" -> (() => InterpreterSchemaUnit.run(testUtil)),
      "PermissionUnit" -> (() => PermissionUnit.run(testUtil)),
      "BlameUnit" -> (() => BlameUnit.run(testUtil)),
      "PageContentUnit" -> (() => PageContentUnit.run(testUtil)),
      "PermissionLogicUnit" -> (() => PermissionLogicUnit.run(testUtil)),
      "WikiPermissionUnit" -> (() => WikiPermissionUnit.run(testUtil)),
      "InterpreterWikiUnit" -> (() => InterpreterWikiUnit.run(testUtil)),
      "CrawlerUnit" -> (() => CrawlerUnit.run(testUtil)),
    )

    unitTests.foreach { case (name, run) =>
      try {
        run()
      } catch {
        case NonFatal(e) =>
          logger.error(s"Unit test failed: $name", e)
          throw e
      }
    }

    Ok(s"Ok (${unitTests.size} tests)")
  }


  def gradient: Action[AnyContent] = devOnly { implicit request =>
    import models.tables.Site
    implicit val site: Site = SiteLogic.get(request.host)
    implicit val contextSite: ContextSite = ContextSite()
    Ok(views.html.Test.gradient(""))
  }
}
