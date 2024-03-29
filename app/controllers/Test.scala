package controllers

import akka.actor.ActorRef
import akka.actor.ActorSystem
import anorm.SQL
import anorm.SqlParser.long
import com.aha00a.commons.Implicits._
import com.aha00a.tests.TestUtil
import logics.PermissionLogic
import logics.SessionLogic
import logics.wikis.WikiPermission
import logics.wikis.interpreters.InterpreterSchema
import logics.wikis.interpreters.InterpreterWiki
import logics.wikis.interpreters.Interpreters
import logics.wikis.interpreters.ahaMark.AhaMarkLink
import models._
import models.tables.Permission
import play.api.Configuration
import play.api.Environment
import play.api.Logging
import play.api.db.Database
import play.api.libs.ws.WSClient
import play.api.mvc._

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
                     configuration: Configuration,
                     wsClient: WSClient,
                     executionContext: ExecutionContext
                    ) extends BaseController with Logging {
  import io.circe.syntax._

  def Ok(json: io.circe.Json): Result = Ok(json.toString()).as(JSON)

  val testUtil = new TestUtil(x => logger.error(x.toString))

  import testUtil.assertEquals

  def unit: Action[AnyContent] = Action { implicit request =>

    import models.tables.Site
    implicit val site: Site = database.withConnection { implicit connection =>
      Site.selectWhereDomain(request.host).getOrElse(Site(-1, ""))
    }
    implicit val contextWikiPage: ContextWikiPage = ContextWikiPage("UnitTest")



    //noinspection DuplicatedCode
    def testInterpreterTable()(implicit request: Request[Any]): Unit = {
      assertEquals(Interpreters.toHtmlString("#!table tsv\na\tb"), <table class="InterpreterTable simpleTable"><tbody><tr><td><div><p>a</p></div></td><td><div><p>b</p></div></td></tr></tbody></table>.toString())
      assertEquals(Interpreters.toHtmlString("#!table\n#!tsv\na\tb"), <table class="InterpreterTable simpleTable"><tbody><tr><td><div><p>a</p></div></td><td><div><p>b</p></div></td></tr></tbody></table>.toString())
      assertEquals(Interpreters.toHtmlString("#!table tsv 1\na\tb"), <table class="InterpreterTable simpleTable"><thead><tr><th><div><p>a</p></div></th><th><div><p>b</p></div></th></tr></thead><tbody></tbody></table>.toString())
      assertEquals(Interpreters.toHtmlString("#!table tsv 0 1\na\tb"), <table class="InterpreterTable simpleTable"><tbody><tr><th><div><p>a</p></div></th><td><div><p>b</p></div></td></tr></tbody></table>.toString())

      assertEquals(Interpreters.toHtmlString("#!table tsv some classes\na\tb"), <table class="InterpreterTable simpleTable some classes"><tbody><tr><td><div><p>a</p></div></td><td><div><p>b</p></div></td></tr></tbody></table>.toString())
      assertEquals(Interpreters.toHtmlString("#!table tsv 1 some classes\na\tb"), <table class="InterpreterTable simpleTable some classes"><thead><tr><th><div><p>a</p></div></th><th><div><p>b</p></div></th></tr></thead><tbody></tbody></table>.toString())
      assertEquals(Interpreters.toHtmlString("#!table tsv 1 tablesorter\na\tb"), <table class="InterpreterTable simpleTable tablesorter"><thead><tr><th><div><p>a</p></div></th><th><div><p>b</p></div></th></tr></thead><tbody></tbody></table>.toString())
      assertEquals(Interpreters.toHtmlString("#!table tsv 0 1 some classes\na\tb"), <table class="InterpreterTable simpleTable some classes"><tbody><tr><th><div><p>a</p></div></th><td><div><p>b</p></div></td></tr></tbody></table>.toString())
    }; testInterpreterTable()

    def testInterpreterWiki()(implicit request: Request[Any]): Unit = {
      import models.tables.Link

      assertEquals(InterpreterWiki.name, "Wiki")

      assertEquals(InterpreterWiki.inlineToHtmlString("""http://a.com"""), """<a href="http://a.com" target="_blank" rel="noopener">http://a.com</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""http://a.com$"""), """<a href="http://a.com$" target="_blank" rel="noopener">http://a.com$</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""[http://a.com]"""), """<a href="http://a.com" target="_blank" rel="noopener">http://a.com</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""[http://a.com a com]"""), """<a href="http://a.com" target="_blank" rel="noopener">a com</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""[FrontPage]"""), """<a href="/w/FrontPage">FrontPage</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""[FrontPage Alias]"""), """<a href="/w/FrontPage">Alias</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""[wiki:FrontPage]"""), """<a href="/w/FrontPage">FrontPage</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""[wiki:FrontPage Alias]"""), """<a href="/w/FrontPage">Alias</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""["Some Page"]"""), """<a href="/w/Some Page" class="missing" rel="nofollow">Some Page</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""["Some Page" Alias]"""), """<a href="/w/Some Page" class="missing" rel="nofollow">Alias</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""[schema:Schema]"""), """<a href="/w/schema:Schema" class="schema">schema:Schema</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""[schema:Schema Alias]"""), """<a href="/w/schema:Schema" class="schema">Alias</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""["schema:Schema"]"""), """<a href="/w/schema:Schema" class="schema">schema:Schema</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""["schema:Schema" Alias]"""), """<a href="/w/schema:Schema" class="schema">Alias</a>""")

      assertEquals(InterpreterWiki.inlineToHtmlString("""http://a.com/$   [http://a.com]  [http://a.com a com]"""), """<a href="http://a.com/$" target="_blank" rel="noopener">http://a.com/$</a>   <a href="http://a.com" target="_blank" rel="noopener">http://a.com</a>  <a href="http://a.com" target="_blank" rel="noopener">a com</a>""")

      assertEquals(InterpreterWiki.inlineToHtmlString("""\http://a.com"""), "http://a.com")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\http://a.com$"""), "http://a.com$")
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
      assertEquals(InterpreterWiki.extractLinkMarkup("""\\[http://a.com]""").toList, Seq(AhaMarkLink("""http://a.com""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""\\[http://a.com a com]""").toList, Seq(AhaMarkLink("""http://a.com""", """a com""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""\\[FrontPage]""").toList, Seq(AhaMarkLink("""FrontPage""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""\\[FrontPage Alias]""").toList, Seq(AhaMarkLink("""FrontPage""", """Alias""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""\\[wiki:FrontPage]""").toList, Seq(AhaMarkLink("""wiki:FrontPage""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""\\[wiki:FrontPage Alias]""").toList, Seq(AhaMarkLink("""wiki:FrontPage""", """Alias""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""\\["SomePage"]""").toList, Seq(AhaMarkLink("""SomePage""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""\\["SomePage" Alias]""").toList, Seq(AhaMarkLink("""SomePage""", """Alias""")))

      assertEquals(InterpreterWiki.toSeqLink("[link]").toList, Seq(Link("UnitTest", "link", "")))
      assertEquals(InterpreterWiki.toSeqLink("[link alias][b]").toList, Seq(Link("UnitTest", "link", "alias"), Link("UnitTest", "b", "")))

    }; testInterpreterWiki()

    def testInterpreterSchema()(implicit request: Request[Any]): Unit = {
      import models.tables.SchemaOrg

      assertEquals(InterpreterSchema.name, "Schema")

      {
        import models.tables.SchemaOrg
        val schemaMarkup =
          """#!Schema Person
            |name	KIM, Aha
            |url	https://aha00a.com
            |memberOf	AhariseNotExists""".stripMargin

        val wikiMarkup =
          s"""[[[$schemaMarkup
             |]]]""".stripMargin

        val interpreted =
          """<div class="schema"><dl vocab="http://schema.org/" typeof="Person">
            |        <h5>
            |          <a class="schema" href="/w/schema:Person">Person</a>
            |        </h5>
            |        <div>
            |          <div>
            |                <dt>
            |                  <a href="/w/schema:name" title="The name of the item." class="">Name</a>
            |                </dt>
            |                <dd property="name"><a href="KIM, Aha" class="missing" rel="nofollow">KIM, Aha</a></dd>
            |              </div><div>
            |                <dt>
            |                  <a href="/w/schema:url" title="URL of the item." class="">Url</a>
            |                </dt>
            |                <dd property="url"><a href="https://aha00a.com" target="_blank" rel="noopener">https://aha00a.com</a></dd>
            |              </div><div>
            |                <dt>
            |                  <a href="/w/schema:memberOf" title="An Organization (or ProgramMembership) to which this Person or Organization belongs." class="">Member Of</a>
            |                </dt>
            |                <dd property="memberOf"><a href="AhariseNotExists" class="missing" rel="nofollow">AhariseNotExists</a></dd>
            |              </div>
            |        </div>
            |      </dl></div>""".stripMargin
        val interpretedWithWiki = s"""<div>$interpreted</div>"""

        assertEquals(InterpreterSchema.toHtmlString(schemaMarkup), interpreted)
        assertEquals(Interpreters.toHtmlString(wikiMarkup), interpretedWithWiki)

        val extractWordResult = Seq("Person", "Name", "KIM,", "Aha", "Url", "https://aha00a.com", "Member", "Of", "AhariseNotExists")
        assertEquals(Interpreters.toText(wikiMarkup).split(" ").toSeq, extractWordResult)

        assertEquals(InterpreterSchema.toSeqLink(schemaMarkup), Seq())
        assertEquals(Interpreters.toSeqLink(wikiMarkup), Seq())

        val extractSchemaResult = Seq(
          SchemaOrg("UnitTest", "Person", "", ""),
          SchemaOrg("UnitTest", "Person", "name", "KIM, Aha"),
          SchemaOrg("UnitTest", "Person", "url", "https://aha00a.com"),
          SchemaOrg("UnitTest", "Person", "memberOf", "AhariseNotExists")
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
          SchemaOrg("UnitTest", "WebApplication", "", ""),
          SchemaOrg("UnitTest", "WebApplication", "name", "AhaWiki"),
          SchemaOrg("UnitTest", "WebApplication", "url", "https://wiki.aha00a.com/w/AhaWiki"),
          SchemaOrg("UnitTest", "WebApplication", "featureList", "https://wiki.aha00a.com/w/AhaWikiFeature"),
          SchemaOrg("UnitTest", "WebApplication", "applicationCategory", "Wiki"),
          SchemaOrg("UnitTest", "WebApplication", "datePublished", "2015-10-21"),
          SchemaOrg("UnitTest", "WebApplication", "datePublished", "2015-10"),
          SchemaOrg("UnitTest", "WebApplication", "datePublished", "--10-21"),
        )
      )
    }; testInterpreterSchema()

    Ok("Ok.")
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
    import logics.ApplicationConf

    import java.net.URL

    val applicationConf = ApplicationConf()
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
    implicit val site: Site = database.withConnection { implicit connection =>
      Site.selectWhereDomain(request.host).getOrElse(Site(-1, ""))
    }
    implicit val context: ContextSite = ContextSite()
    Ok(views.html.Test.gradient(""))
  }

  def permission: Action[AnyContent] = Action { implicit request =>database.withConnection { implicit connection =>

    import models.ContextSite.RequestWrapper
    import models.tables.Site
    import play.api.Mode

    implicit val site: Site = Site.selectWhereDomain(request.host).getOrElse(Site(-1, ""))
    implicit val contextWikiPage: ContextWikiPage = ContextWikiPage("")
    implicit val provider: RequestWrapper = contextWikiPage.requestWrapper

    val q = "#!read";
    val wikiPermission = WikiPermission()
    val id = SessionLogic.getId(request).getOrElse("")
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
              val readable = permissionLogic.permitted(sr.name, id, Permission.read)
              isReadableFromLegacy != readable && isReadableFromLegacy
            }).map(_.name),
          "readNewOnly" -> seqSearchResult
            .filter(sr => {
              val pageContent = PageContent(sr.content)
              val isReadableFromLegacy = wikiPermission.isReadable(pageContent)
              val readable = permissionLogic.permitted(sr.name, id, Permission.read)
              isReadableFromLegacy != readable && readable
            }).map(_.name),
          "writeLegacyOnly" -> seqSearchResult
            .filter(sr => {
              val pageContent = PageContent(sr.content)
              val isWritableFromLagacy = wikiPermission.isWritable(pageContent)
              val editable = permissionLogic.permitted(sr.name, id, Permission.edit)
              isWritableFromLagacy != editable && isWritableFromLagacy
            }).map(_.name),
          "writeNewOnly" -> seqSearchResult
            .filter(sr => {
              val pageContent = PageContent(sr.content)
              val isWritableFromLagacy = wikiPermission.isWritable(pageContent)
              val editable = permissionLogic.permitted(sr.name, id, Permission.edit)
              isWritableFromLagacy != editable && editable
            }).map(_.name),
        )
      }
    ).getOrElse(Map())

    Ok(map.asJson)
  }}
}



