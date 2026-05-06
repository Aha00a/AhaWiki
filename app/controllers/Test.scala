package controllers
import akka.actor.ActorRef
import akka.actor.ActorSystem
import anorm.SQL
import anorm.SqlParser.long
import com.aha00a.commons.Implicits._
import com.aha00a.tests.TestUtil
import com.aha00a.tests.unit.{HeadingNumberUnit, InterpreterVimUnit, MacroPeriodUnit, SchemaOrgUnit, UrlDetectorUnit, WikiMacrosUnit}
import logics.AhaWikiCache
import logics.ApplicationConf
import logics.PermissionLogic
import logics.SessionLogic
import logics.SiteLogic
import logics.wikis.WikiPermission
import logics.wikis.interpreters.InterpreterSchema
import logics.wikis.interpreters.InterpreterWiki
import logics.wikis.interpreters.Interpreters
import logics.wikis.interpreters.ahaMark.AhaMarkLink
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

    //noinspection DuplicatedCode
    def testInterpreterTable()(implicit request: Request[Any]): Unit = {
      assertEquals(Interpreters.toHtmlString("#!table tsv\na\tb"), <table class="InterpreterTable simpleTable tablesorter"><tbody><tr><td><div><p>a</p></div></td><td><div><p>b</p></div></td></tr></tbody></table>.toString())
      assertEquals(Interpreters.toHtmlString("#!table\n#!tsv\na\tb"), <table class="InterpreterTable simpleTable tablesorter"><tbody><tr><td><div><p>a</p></div></td><td><div><p>b</p></div></td></tr></tbody></table>.toString())
      assertEquals(Interpreters.toHtmlString("#!table tsv 1\na\tb"), <table class="InterpreterTable simpleTable tablesorter"><thead><tr><th><div><p>a</p></div></th><th><div><p>b</p></div></th></tr></thead><tbody></tbody></table>.toString())
      assertEquals(Interpreters.toHtmlString("#!table tsv 0 1\na\tb"), <table class="InterpreterTable simpleTable tablesorter"><tbody><tr><th><div><p>a</p></div></th><td><div><p>b</p></div></td></tr></tbody></table>.toString())

      assertEquals(Interpreters.toHtmlString("#!table tsv some classes\na\tb"), <table class="InterpreterTable simpleTable tablesorter some classes"><tbody><tr><td><div><p>a</p></div></td><td><div><p>b</p></div></td></tr></tbody></table>.toString())
      assertEquals(Interpreters.toHtmlString("#!table tsv 1 some classes\na\tb"), <table class="InterpreterTable simpleTable tablesorter some classes"><thead><tr><th><div><p>a</p></div></th><th><div><p>b</p></div></th></tr></thead><tbody></tbody></table>.toString())
      assertEquals(Interpreters.toHtmlString("#!table tsv 1 tablesorter\na\tb"), <table class="InterpreterTable simpleTable tablesorter"><thead><tr><th><div><p>a</p></div></th><th><div><p>b</p></div></th></tr></thead><tbody></tbody></table>.toString())
      assertEquals(Interpreters.toHtmlString("#!table tsv 0 1 some classes\na\tb"), <table class="InterpreterTable simpleTable tablesorter some classes"><tbody><tr><th><div><p>a</p></div></th><td><div><p>b</p></div></td></tr></tbody></table>.toString())
      assertEquals(Interpreters.toHtmlString("#!table tsv 1 some \"bad\" classes\na\tb"), <table class="InterpreterTable simpleTable tablesorter some classes"><thead><tr><th><div><p>a</p></div></th><th><div><p>b</p></div></th></tr></thead><tbody></tbody></table>.toString())
      assertEquals(Interpreters.toHtmlString("#!table tsv 1 \"bad\"\na\tb"), <table class="InterpreterTable simpleTable tablesorter"><thead><tr><th><div><p>a</p></div></th><th><div><p>b</p></div></th></tr></thead><tbody></tbody></table>.toString())
      assertEquals(Interpreters.toHtmlString("#!table csv\n\"a,b\",c"), <table class="InterpreterTable simpleTable tablesorter"><tbody><tr><td><div><p>a,b</p></div></td><td><div><p>c</p></div></td></tr></tbody></table>.toString())
    }; testInterpreterTable()

    def testInterpreterWiki()(implicit request: Request[Any]): Unit = {
      import models.tables.CalculatedLink

      assertEquals(InterpreterWiki.name, "Wiki")

      assertEquals(InterpreterWiki.inlineToHtmlString("""http://a.com"""), """<a href="http://a.com" target="_blank" rel="noopener">http://a.com</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""http://a.com$"""), """<a href="http://a.com$" target="_blank" rel="noopener">http://a.com$</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""http://a.com/some$thing"""), """<a href="http://a.com/some$thing" target="_blank" rel="noopener">http://a.com/some$thing</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""[http://a.com]"""), """<a href="http://a.com" target="_blank" rel="noopener">http://a.com</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""[http://a.com a com]"""), """<a href="http://a.com" target="_blank" rel="noopener">a com</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""[FrontPage]"""), """<a href="/w/FrontPage">FrontPage</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""[FrontPage Alias]"""), """<a href="/w/FrontPage">Alias</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""[wiki:FrontPage]"""), """<a href="/w/FrontPage">FrontPage</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""[wiki:FrontPage Alias]"""), """<a href="/w/FrontPage">Alias</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""["Some Page"]"""), """<a href="/w/Some Page" class="missing" rel="nofollow">Some Page</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""["Some Page" Alias]"""), """<a href="/w/Some Page" class="missing" rel="nofollow">Alias</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""[schema:Schema]"""), """<a href="/w/schema:Schema" class="schema schema-link schema-schema">schema:Schema</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""[schema:Movie]"""), """<a href="/w/schema:Movie" class="schema schema-link schema-movie">schema:Movie</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""[schema:Schema Alias]"""), """<a href="/w/schema:Schema" class="schema schema-link schema-schema">Alias</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""["schema:Schema"]"""), """<a href="/w/schema:Schema" class="schema schema-link schema-schema">schema:Schema</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""["schema:Schema" Alias]"""), """<a href="/w/schema:Schema" class="schema schema-link schema-schema">Alias</a>""")

      assertEquals(InterpreterWiki.inlineToHtmlString("""[KR]"""), """<a href="/w/KR" class="iso3166-alpha2-link">🇰🇷 KR</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""[US United States]"""), """<a href="/w/US" class="iso3166-alpha2-link">🇺🇸 United States</a>""")
      assertEquals(AhaMarkLink("""JP""").toHtmlString(), """<a href="/w/JP" class="iso3166-alpha2-link">🇯🇵 JP</a>""")

      assertEquals(InterpreterWiki.inlineToHtmlString("""http://a.com/$   [http://a.com]  [http://a.com a com]"""), """<a href="http://a.com/$" target="_blank" rel="noopener">http://a.com/$</a>   <a href="http://a.com" target="_blank" rel="noopener">http://a.com</a>  <a href="http://a.com" target="_blank" rel="noopener">a com</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""http://a.com/some$thing   [http://a.com]  [http://a.com a com]"""), """<a href="http://a.com/some$thing" target="_blank" rel="noopener">http://a.com/some$thing</a>   <a href="http://a.com" target="_blank" rel="noopener">http://a.com</a>  <a href="http://a.com" target="_blank" rel="noopener">a com</a>""")

      assertEquals(InterpreterWiki.inlineToHtmlString("""\http://a.com"""), "http://a.com")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\http://a.com$"""), "http://a.com$")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\http://a.com/some$thing"""), "http://a.com/some$thing")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\[http://a.com]"""), "[http://a.com]")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\[http://a.com a com]"""), "[http://a.com a com]")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\[FrontPage]"""), "[FrontPage]")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\[FrontPage Alias]"""), "[FrontPage Alias]")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\[wiki:FrontPage]"""), "[wiki:FrontPage]")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\[wiki:FrontPage Alias]"""), "[wiki:FrontPage Alias]")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\[wiki:FrontPage]"""), "[wiki:FrontPage]")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\[wiki:FrontPage Alias]"""), "[wiki:FrontPage Alias]")

      assertEquals(InterpreterWiki.inlineToHtmlString("""\\http://a.com"""), """\\<a href="http://a.com" target="_blank" rel="noopener">http://a.com</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\\http://a.com$"""), """\\<a href="http://a.com$" target="_blank" rel="noopener">http://a.com$</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\\http://a.com/some$thing"""), """\\<a href="http://a.com/some$thing" target="_blank" rel="noopener">http://a.com/some$thing</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\\[http://a.com]"""), """\\<a href="http://a.com" target="_blank" rel="noopener">http://a.com</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\\[http://a.com a com]"""), """\\<a href="http://a.com" target="_blank" rel="noopener">a com</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\\[FrontPage]"""), """\\<a href="/w/FrontPage">FrontPage</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\\[FrontPage Alias]"""), """\\<a href="/w/FrontPage">Alias</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\\[wiki:FrontPage]"""), """\\<a href="/w/FrontPage">FrontPage</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\\[wiki:FrontPage Alias]"""), """\\<a href="/w/FrontPage">Alias</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\\["Some Page"]"""), """\\<a href="/w/Some Page" class="missing" rel="nofollow">Some Page</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\\["Some Page" Alias]"""), """\\<a href="/w/Some Page" class="missing" rel="nofollow">Alias</a>""")


      assertEquals(AhaMarkLink("""http://example.com""").toHtmlString(), """<a href="http://example.com" target="_blank" rel="noopener">http://example.com</a>""")
      assertEquals(AhaMarkLink("""AhaWiki""").toHtmlString(), """<a href="/w/AhaWiki">AhaWiki</a>""")
      assertEquals(AhaMarkLink("""#AhaWiki""").toHtmlString(), """<a href="#AhaWiki">#AhaWiki</a>""")
      assertEquals(AhaMarkLink("""?q=1""").toHtmlString(), """<a href="?q=1">?q=1</a>""")
      assertEquals(AhaMarkLink("""With:Colon""").toHtmlString(), """<a href="/w/With:Colon">With:Colon</a>""")
      assertEquals(AhaMarkLink("""With: Colon""").toHtmlString(), """<a href="/w/With: Colon">With: Colon</a>""")


      assertEquals(InterpreterWiki.extractLinkMarkup("""http://a.com""").toList, Seq(AhaMarkLink("""http://a.com""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""http://a.com$""").toList, Seq(AhaMarkLink("""http://a.com$""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""http://a.com/some$thing""").toList, Seq(AhaMarkLink("""http://a.com/some$thing""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""[http://a.com]""").toList, Seq(AhaMarkLink("""http://a.com""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""[http://a.com a com]""").toList, Seq(AhaMarkLink("""http://a.com""", """a com""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""[FrontPage]""").toList, Seq(AhaMarkLink("""FrontPage""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""[FrontPage Alias]""").toList, Seq(AhaMarkLink("""FrontPage""", """Alias""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""[wiki:FrontPage]""").toList, Seq(AhaMarkLink("""wiki:FrontPage""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""[wiki:FrontPage Alias]""").toList, Seq(AhaMarkLink("""wiki:FrontPage""", """Alias""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""["Some Page"]""").toList, Seq(AhaMarkLink("""Some Page""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""["Some Page" Alias]""").toList, Seq(AhaMarkLink("""Some Page""", "Alias")))

      assertEquals(
        InterpreterWiki.extractLinkMarkup("""http://a.com/$   [http://a.com]  [http://a.com a com]""").toList,
        Seq(AhaMarkLink("""http://a.com/$"""), AhaMarkLink("""http://a.com"""), AhaMarkLink("""http://a.com""", """a com"""))
      )

      assertEquals(InterpreterWiki.extractLinkMarkup("""\http://a.com""").toList, Seq())
      assertEquals(InterpreterWiki.extractLinkMarkup("""\http://a.com$""").toList, Seq())
      assertEquals(InterpreterWiki.extractLinkMarkup("""\http://a.com/some$thing""").toList, Seq())
      assertEquals(InterpreterWiki.extractLinkMarkup("""\[http://a.com]""").toList, Seq())
      assertEquals(InterpreterWiki.extractLinkMarkup("""\[http://a.com a com]""").toList, Seq())
      assertEquals(InterpreterWiki.extractLinkMarkup("""\[FrontPage]""").toList, Seq())
      assertEquals(InterpreterWiki.extractLinkMarkup("""\[FrontPage Alias]""").toList, Seq())
      assertEquals(InterpreterWiki.extractLinkMarkup("""\[wiki:FrontPage]""").toList, Seq())
      assertEquals(InterpreterWiki.extractLinkMarkup("""\[wiki:FrontPage Alias]""").toList, Seq())
      assertEquals(InterpreterWiki.extractLinkMarkup("""\["SomePage"]""").toList, Seq())
      assertEquals(InterpreterWiki.extractLinkMarkup("""\["SomePage" Alias]""").toList, Seq())

      assertEquals(InterpreterWiki.extractLinkMarkup("""\\http://a.com""").toList, Seq(AhaMarkLink("""http://a.com""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""\\http://a.com$""").toList, Seq(AhaMarkLink("""http://a.com$""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""\\http://a.com/some$thing""").toList, Seq(AhaMarkLink("""http://a.com/some$thing""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""\\[http://a.com]""").toList, Seq(AhaMarkLink("""http://a.com""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""\\[http://a.com a com]""").toList, Seq(AhaMarkLink("""http://a.com""", """a com""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""\\[FrontPage]""").toList, Seq(AhaMarkLink("""FrontPage""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""\\[FrontPage Alias]""").toList, Seq(AhaMarkLink("""FrontPage""", """Alias""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""\\[wiki:FrontPage]""").toList, Seq(AhaMarkLink("""wiki:FrontPage""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""\\[wiki:FrontPage Alias]""").toList, Seq(AhaMarkLink("""wiki:FrontPage""", """Alias""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""\\["SomePage"]""").toList, Seq(AhaMarkLink("""SomePage""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""\\["SomePage" Alias]""").toList, Seq(AhaMarkLink("""SomePage""", """Alias""")))

      assertEquals(InterpreterWiki.toSeqLink("[link]").toList, Seq(CalculatedLink("UnitTest", "link", "")))
      assertEquals(InterpreterWiki.toSeqLink("[link alias][b]").toList, Seq(CalculatedLink("UnitTest", "link", "alias"), CalculatedLink("UnitTest", "b", "")))

    }; testInterpreterWiki()

    HeadingNumberUnit.run(testUtil)
    InterpreterVimUnit.run(testUtil)
    WikiMacrosUnit.run(testUtil)
    MacroPeriodUnit.run(testUtil)
    UrlDetectorUnit.run(testUtil)
    SchemaOrgUnit.run(testUtil)

    def testInterpreterSchema()(implicit request: Request[Any]): Unit = {
      import models.tables.CalculatedSchemaOrg

      assertEquals(InterpreterSchema.name, "Schema")

      {
        import models.tables.CalculatedSchemaOrg
        val schemaMarkup =
          """#!Schema Person
            |name	KIM, Aha
            |url	https://aha00a.com
            |memberOf	AhariseNotExists""".stripMargin

        val wikiMarkup =
          s"""[[[$schemaMarkup
             |]]]""".stripMargin

        val currentPageUrl = s"https://${request.host}/w/UnitTest"
        val interpreted =
          s"""<div class="schema InterpreterSchema"><dl vocab="https://schema.org/" typeof="Person">
            |        <h5 class="schemaClassTitle">
            |          <a class="schema schema-link schema-person" href="/w/schema:Person">Person</a>
            |        </h5>
            |        <div class="schemaFields">
            |          <div class="schemaFieldRow">
            |                <dt class="schemaFieldKey">
            |                  <a href="/w/schema:name" title="The name of the item." class="">Name</a>
            |                </dt>
            |                <dd class="schemaFieldValue" property="name"><a rel="nofollow" class="missing" href="/w/KIM, Aha">KIM, Aha</a></dd>
            |              </div><div class="schemaFieldRow">
            |                <dt class="schemaFieldKey">
            |                  <a href="/w/schema:url" title="URL of the item." class="">Url</a>
            |                </dt>
            |                <dd class="schemaFieldValue" property="url"><a rel="noopener" target="_blank" href="https://aha00a.com">https://aha00a.com</a></dd>
            |              </div><div class="schemaFieldRow">
            |                <dt class="schemaFieldKey">
            |                  <a href="/w/schema:memberOf" title="An Organization (or ProgramMembership) to which this Person or Organization belongs." class="">Member Of</a>
            |                </dt>
            |                <dd class="schemaFieldValue" property="memberOf"><a rel="nofollow" class="missing" href="/w/AhariseNotExists">AhariseNotExists</a></dd>
            |              </div>
            |        </div>
            |      </dl><script type="application/ld+json">{"@context":"https://schema.org","@type":"Person","mainEntityOfPage":"$currentPageUrl","@id":"$currentPageUrl","inLanguage":"en-US","name":"KIM, Aha","memberOf":"AhariseNotExists","url":"https://aha00a.com"}</script></div>""".stripMargin
        val interpretedWithWiki =
          s"""<div><div class="InterpreterRenderMetaWrapper" style="position: relative;"
             |  data-edit-link="/w/UnitTest?action=edit&revision=0&lineStart=1&lineEnd=6"
             |  data-line-start="1"
             |  data-line-end="6"
             |  data-edit-title="Edit (r0, L1-L5)">
             |  <div class="InterpreterRenderContent">$interpreted</div>
             |</div></div>""".stripMargin

        assertEquals(InterpreterSchema.toHtmlString(schemaMarkup), interpreted)
        assertEquals(Interpreters.toHtmlString(wikiMarkup), interpretedWithWiki)

        val extractWordResult = Seq("Person", "Name", "KIM,", "Aha", "Url", "https://aha00a.com", "Member", "Of", "AhariseNotExists")
        assertEquals(Interpreters.toText(wikiMarkup).split(" ").toSeq, extractWordResult)

        assertEquals(InterpreterSchema.toSeqLink(schemaMarkup), Seq())
        assertEquals(Interpreters.toSeqLink(wikiMarkup), Seq())

        val extractSchemaResult = Seq(
          CalculatedSchemaOrg("UnitTest", "Person", "", "schema:Person"),
          CalculatedSchemaOrg("UnitTest", "Person", "name", "KIM, Aha"),
          CalculatedSchemaOrg("UnitTest", "Person", "memberOf", "AhariseNotExists")
        )
        assertEquals(InterpreterSchema.toSeqSchemaOrg(schemaMarkup), extractSchemaResult)
        assertEquals(Interpreters.toSeqSchemaOrg(wikiMarkup), extractSchemaResult)
      }


      assertEquals(
        InterpreterSchema.toSeqSchemaOrg(
          """#!Schema WebApplication
            |name	AhaWiki
            |url	https://wiki.aha00a.com/w/AhaWiki
            |featureList	https://wiki.aha00a.com/w/AhaWikiFeature
            |applicationCategory	Wiki
            |datePublished	2015-10-21
            |""".stripMargin
        ).toList,
        Seq(
          CalculatedSchemaOrg("UnitTest", "WebApplication", "", "schema:WebApplication"),
          CalculatedSchemaOrg("UnitTest", "WebApplication", "name", "AhaWiki"),
          CalculatedSchemaOrg("UnitTest", "WebApplication", "applicationCategory", "Wiki"),
          CalculatedSchemaOrg("UnitTest", "WebApplication", "datePublished", "2015-10-21"),
          CalculatedSchemaOrg("UnitTest", "WebApplication", "datePublished", "2015-10"),
        )
      )
    }; testInterpreterSchema()

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
