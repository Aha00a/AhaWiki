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
import logics.PermissionLogic
import logics.SessionLogic
import logics.SiteLogic
import logics.wikis.WikiPermission
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
import scala.sys.process.{Process, ProcessLogger}

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
    val sessionMaxAge = configuration.getOptional[Long]("play.http.session.maxAge")
    logger.info(s"[/test/unit] play.http.session.maxAge=$sessionMaxAge")
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

    def runCommand(command: Seq[String], label: String): Unit = {
      logger.info(s"[/test/unit] Running $label: ${command.mkString(" ")}")
      val stdOut = new StringBuilder
      val stdErr = new StringBuilder
      val exitCode = Process(command, new File(".")).!(ProcessLogger(
        out => stdOut.append(out).append("\n"),
        err => stdErr.append(err).append("\n"),
      ))
      if (exitCode != 0) {
        val output = (stdOut.toString + stdErr.toString).take(4000)
        throw new RuntimeException(s"$label failed (exit=$exitCode). output=$output")
      }
    }

    case class TestExecutionResult(label: String, discovered: Int, executed: Int, skipped: Boolean) {
      def toSummaryLine: String = {
        val status = if (skipped) "SKIPPED" else "PASS"
        s"$label[$status] discovered=$discovered executed=$executed"
      }
    }



    def runJavaScriptUnitTests(): TestExecutionResult = {
      val testDir = new File("test")
      val jsTests = Option(testDir.listFiles()).toSeq.flatten
        .filter(file => file.isFile && file.getName.endsWith(".test.mjs"))
        .sortBy(_.getName)
        .map(file => s"test/${file.getName}")

      if (jsTests.nonEmpty) {
        runCommand(Seq("node", "--test") ++ jsTests, "JavaScript unit tests")
        TestExecutionResult("js", jsTests.size, jsTests.size, skipped = false)
      } else {
        logger.warn("[/test/unit] No JavaScript test files (*.test.mjs) found under ./test; skipping JS suite run.")
        TestExecutionResult("js", discovered = 0, executed = 0, skipped = true)
      }
    }

    val jsResult = runJavaScriptUnitTests()

    val fileAbsolute = new File(".").getAbsoluteFile
    val total = fileAbsolute.getTotalSpace / 1024.0 / 1024
    val free = fileAbsolute.getFreeSpace / 1024.0 / 1024
    val percent = free / total * 100
    val diskMessage: String = f"${free}%,.0f MiB / ${total}%,.0f MiB = $percent%.2f%% free"
    val testSummary = Seq(jsResult.toSummaryLine).mkString("\n")
    val message = s"$testSummary\ndisk=$diskMessage"
    if(percent < 5) InsufficientStorage(message) else Ok(message)
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

  def permission: Action[AnyContent] = Action { implicit request =>database.withConnection { implicit connection =>

    import models.RequestWrapper
    import models.tables.Site
    import play.api.Mode

    implicit val site: Site = SiteLogic.get(request.host)
    implicit val contextWikiPage: ContextWikiPage = ContextWikiPage("")
    implicit val provider: RequestWrapper = contextWikiPage.requestWrapper

    val q = "#!read"
    val wikiPermission = WikiPermission()
    val email = SessionLogic.getUser(request).map(_.email).getOrElse("")
    val seqPermission = if(environment.mode == Mode.Dev) Permission.select() else Seq() // TODO:
    val permissionLogic = new PermissionLogic(seqPermission)

    val map = q.toOption.map(
      q => {
        import models.tables.SearchResult
        val seqSearchResult: Seq[SearchResult] = models.tables.Page.pageSearch(q)
        Map(
          "readLegacyOnly" -> seqSearchResult
            .filter(sr => {
              val pageContent = PageContent(sr.content)
              val isReadableFromLegacy = wikiPermission.isReadable(pageContent)
              val readable = permissionLogic.permitted(sr.name, email, Permission.read)
              isReadableFromLegacy != readable && isReadableFromLegacy
            }).map(_.name),
          "readNewOnly" -> seqSearchResult
            .filter(sr => {
              val pageContent = PageContent(sr.content)
              val isReadableFromLegacy = wikiPermission.isReadable(pageContent)
              val readable = permissionLogic.permitted(sr.name, email, Permission.read)
              isReadableFromLegacy != readable && readable
            }).map(_.name),
          "writeLegacyOnly" -> seqSearchResult
            .filter(sr => {
              val pageContent = PageContent(sr.content)
              val isWritableFromLagacy = wikiPermission.isWritable(pageContent)
              val editable = permissionLogic.permitted(sr.name, email, Permission.edit)
              isWritableFromLagacy != editable && isWritableFromLagacy
            }).map(_.name),
          "writeNewOnly" -> seqSearchResult
            .filter(sr => {
              val pageContent = PageContent(sr.content)
              val isWritableFromLagacy = wikiPermission.isWritable(pageContent)
              val editable = permissionLogic.permitted(sr.name, email, Permission.edit)
              isWritableFromLagacy != editable && editable
            }).map(_.name),
        )
      }
    ).getOrElse(Map())

    Ok(map.asJson)
  }}
}
