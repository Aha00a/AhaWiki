package controllers
import akka.actor.ActorRef
import akka.actor.ActorSystem
import anorm.SQL
import anorm.SqlParser.long
import com.aha00a.commons.Implicits._
import com.aha00a.tests.TestUtil
import com.aha00a.tests.unit.{BlameUnit, HeadingNumberUnit, InterpreterBlockUnit, InterpreterMarkdownUnit, InterpreterSchemaUnit, InterpreterVimUnit, InterpreterWikiUnit, JsonUnit, MacroPeriodUnit, PageContentUnit, PermissionLogicUnit, PermissionUnit, SchemaOrgUnit, SignedReadUrlLogicUnit, TraitInterpreterUnit, UrlDetectorUnit, WikiMacrosUnit}
import logics.AhaWikiCache
import logics.ApplicationConf
import logics.Crawler
import logics.PermissionLogic
import logics.SiteLogic
import logics.wikis.interpreters.InterpreterSchema
import models._
import models.tables.Permission
import models.tables.Site
import play.api.{Configuration, Environment}
import play.api.Logging
import play.api.db.Database
import play.api.libs.ws.WSClient
import play.api.mvc._

import java.io.File
import java.util.Date
import javax.inject.Inject
import javax.inject.Named
import scala.concurrent.ExecutionContext

class Test @Inject()(implicit val
                     controllerComponents: ControllerComponents,
                     actorSystem: ActorSystem,
                     database: Database,
                     environment: Environment,
                     @Named("db-actor") actorAhaWiki: ActorRef,
                     applicationConf: ApplicationConf,
                     configuration: Configuration,
                     ahaWikiCache: AhaWikiCache,
                     wsClient: WSClient,
                     executionContext: ExecutionContext
                    ) extends BaseController with Logging {
  import io.circe.syntax._

  def Ok(json: io.circe.Json): Result = Ok(json.toString()).as(JSON)

  val testUtil = new TestUtil(x => logger.error(x.toString))

  import testUtil.assertEquals

  def unit: Action[AnyContent] = Action { implicit request =>
    implicit val site: Site = SiteLogic.get(request.host)
    implicit val contextWikiPage: ContextWikiPage = ContextWikiPage("UnitTest")

    InterpreterBlockUnit.run(testUtil)
    HeadingNumberUnit.run(testUtil)
    InterpreterVimUnit.run(testUtil)
    WikiMacrosUnit.run(testUtil)
    MacroPeriodUnit.run(testUtil)
    UrlDetectorUnit.run(testUtil)
    SchemaOrgUnit.run(testUtil)
    TraitInterpreterUnit.run(testUtil)
    SignedReadUrlLogicUnit.run(testUtil)
    InterpreterMarkdownUnit.run(testUtil)
    JsonUnit.run(testUtil)
    InterpreterSchemaUnit.run(testUtil)
    PermissionUnit.run(testUtil)
    BlameUnit.run(testUtil)
    PageContentUnit.run(testUtil)
    PermissionLogicUnit.run(testUtil)
    InterpreterWikiUnit.run(testUtil)
    apiCrawlerUnit()

    val fileAbsolute = new File(".").getAbsoluteFile
    val total = fileAbsolute.getTotalSpace / 1024.0 / 1024
    val free = fileAbsolute.getFreeSpace / 1024.0 / 1024
    val percent = free / total * 100
    val message: String = f"${free}%,.0f MiB / ${total}%,.0f MiB = $percent%.2f%% free"
    if(percent < 5) InsufficientStorage(message) else Ok(message)
  }


  // TODO: extract to CrawlerUnit.run
  private def apiCrawlerUnit() = {
    {
      val crawler = Crawler.fromHtml(
        """<html>
          |<head>
          |  <title>title</title>
          |</head>
          |<body>body</body>
          |</html>
        """.stripMargin)
      assertEquals(crawler.title, "title")
      assertEquals(crawler.description, "body")
      assertEquals(crawler.image, "")
    }
    {
      val crawler = Crawler.fromHtml(
        """<html>
          |<head>
          |  <title>title</title>
          |  <meta property="og:title" content="ogTitle">
          |  <meta property="og:description" content="ogDescription">
          |  <meta property="og:image" content="ogImage">
          |</head>
          |<body>body</body>
          |</html>
        """.stripMargin)
      assertEquals(crawler.title, "ogTitle")
      assertEquals(crawler.description, "ogDescription")
      assertEquals(crawler.image, "ogImage")
    }
  }


  case class Dddd()(implicit database2: Database) {
    def selectCount(): Long = database2.withConnection { implicit connection =>
      //noinspection LanguageFeature
      SQL("SELECT COUNT(*) cnt FROM Page").as(long("cnt") single)
    }
  }

  def dbtest: Action[AnyContent] = Action { implicit request =>
    Ok(Dddd().selectCount().toString + "aa")
  }

  def filetest: Action[AnyContent] = Action { implicit request =>
    import com.amazonaws.HttpMethod
    import com.amazonaws.auth.AWSStaticCredentialsProvider
    import com.amazonaws.auth.BasicAWSCredentials
    import com.amazonaws.services.s3.AmazonS3
    import com.amazonaws.services.s3.AmazonS3ClientBuilder

    import java.net.URL

    val credentials = new BasicAWSCredentials(
      applicationConf.AhaWiki.aws.AWS_ACCESS_KEY_ID(),
      applicationConf.AhaWiki.aws.AWS_SECRET_ACCESS_KEY(),
    )
    val amazonS3: AmazonS3 = AmazonS3ClientBuilder.standard.withCredentials(new AWSStaticCredentialsProvider(credentials)).withRegion(applicationConf.AhaWiki.aws.AWS_REGION()).build

    val bucket = applicationConf.AhaWiki.aws.s3.bucket()
    val key = "/Iron Man/poster.jpg"
    val dateExpiration = new Date(new Date().getTime + 1000 * 60 * 5)
    val url: URL = amazonS3.generatePresignedUrl(bucket, key, dateExpiration, HttpMethod.GET)

    Ok("Ok. - " + url)
  }


  def gradient: Action[AnyContent] = Action { implicit request =>
    import models.tables.Site
    implicit val site: Site = SiteLogic.get(request.host)
    implicit val contextSite: ContextSite = ContextSite()
    Ok(views.html.Test.gradient(""))
  }

  def permission: Action[AnyContent] = Action { implicit request => database.withConnection { implicit connection =>
    val siteSeq = request.getQueryString("siteSeq").flatMap(v => scala.util.Try(v.trim.toLong).toOption)
    implicit val site: Site = siteSeq.flatMap(SiteLogic.get(_)(database)).getOrElse(SiteLogic.get(request.host))
    val pageName = request.getQueryString("pageName").map(_.trim).getOrElse("")
    val actor = request.getQueryString("actor").map(_.trim).getOrElse("")
    val action = request.getQueryString("action").map(_.trim).filter(_.nonEmpty).getOrElse("read")
    val requiredAction = Permission.parseAction(action).getOrElse(Permission.read)
    val logic = new PermissionLogic(Permission.select())
    val matched = logic.matched(pageName, actor)

    val json = io.circe.Json.obj(
      "siteSeq" -> io.circe.Json.fromLong(site.seq),
      "pageName" -> io.circe.Json.fromString(pageName),
      "actor" -> io.circe.Json.fromString(actor),
      "requiredAction" -> io.circe.Json.fromInt(requiredAction),
      "permitted" -> io.circe.Json.fromBoolean(matched.exists(_.permitted(requiredAction))),
      "matchedPermission" -> matched.map { permission =>
        io.circe.Json.obj(
          "targetType" -> io.circe.Json.fromString(permission.targetType.toString),
          "target" -> io.circe.Json.fromString(permission.target),
          "actorType" -> io.circe.Json.fromString(permission.actorType.toString),
          "actor" -> io.circe.Json.fromString(permission.actor),
          "action" -> io.circe.Json.fromInt(permission.action),
        )
      }.getOrElse(io.circe.Json.Null),
      "permissionCount" -> io.circe.Json.fromInt(logic.seq.size),
    )

    Ok(json)
  }}
}
