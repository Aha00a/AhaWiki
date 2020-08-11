package controllers

import akka.actor.ActorRef
import akka.actor.ActorSystem
import anorm.SQL
import anorm.SqlParser.long
import com.aha00a.commons.Implicits._
import com.aha00a.tests.TestUtil
import javax.inject.Inject
import logics.wikis.interpreters.InterpreterSchema
import logics.wikis.interpreters.InterpreterWiki
import logics.wikis.interpreters.InterpreterWiki.LinkMarkup
import logics.wikis.interpreters.Interpreters
import models._
import play.api.Configuration
import play.api.Logging
import play.api.cache.SyncCacheApi
import play.api.db.Database
import play.api.mvc._

class Test @Inject()(implicit val
                     controllerComponents: ControllerComponents,
                     syncCacheApi: SyncCacheApi,
                     system: ActorSystem,
                     database: play.api.db.Database,
                     @javax.inject.Named("db-actor") actorAhaWiki: ActorRef,
                     configuration: Configuration
                    ) extends BaseController with Logging {
  val testUtil = new TestUtil(x => logger.error(x.toString))

  import testUtil.assertEquals

  def unit: Action[AnyContent] = Action { implicit request =>

    implicit val wikiContext: WikiContext = WikiContext("UnitTest")

    def testMacroMonths()(implicit wikiContext: WikiContext): Unit = {
      import logics.wikis.macros.MacroMonths
      assertEquals(MacroMonths.toHtmlString("1000"),
        """<ul style="list-style-type: disc;">
          |<li><a href="1000-01">1000-01</a></li>
          |<li><a href="1000-02">1000-02</a></li>
          |<li><a href="1000-03">1000-03</a></li>
          |<li><a href="1000-04">1000-04</a></li>
          |<li><a href="1000-05">1000-05</a></li>
          |<li><a href="1000-06">1000-06</a></li>
          |<li><a href="1000-07">1000-07</a></li>
          |<li><a href="1000-08">1000-08</a></li>
          |<li><a href="1000-09">1000-09</a></li>
          |<li><a href="1000-10">1000-10</a></li>
          |<li><a href="1000-11">1000-11</a></li>
          |<li><a href="1000-12">1000-12</a></li>
          |</ul>""".stripMargin)
      assertEquals(MacroMonths.extractLink("1000"), "1000-01,1000-02,1000-03,1000-04,1000-05,1000-06,1000-07,1000-08,1000-09,1000-10,1000-11,1000-12".split(',').toSeq)
    }; testMacroMonths()


    //noinspection DuplicatedCode
    def testInterpreterTable()(implicit request: Request[Any], syncCacheApi: SyncCacheApi): Unit = {
      assertEquals(Interpreters.toHtmlString("#!table tsv\na\tb"), <table class="InterpreterTable simpleTable"><tbody><tr><td><p>a</p></td><td><p>b</p></td></tr></tbody></table>.toString())
      assertEquals(Interpreters.toHtmlString("#!table\n#!tsv\na\tb"), <table class="InterpreterTable simpleTable"><tbody><tr><td><p>a</p></td><td><p>b</p></td></tr></tbody></table>.toString())
      assertEquals(Interpreters.toHtmlString("#!table tsv 1\na\tb"), <table class="InterpreterTable simpleTable"><thead><tr><th><p>a</p></th><th><p>b</p></th></tr></thead><tbody></tbody></table>.toString())
      assertEquals(Interpreters.toHtmlString("#!table tsv 0 1\na\tb"), <table class="InterpreterTable simpleTable"><tbody><tr><th><p>a</p></th><td><p>b</p></td></tr></tbody></table>.toString())

      assertEquals(Interpreters.toHtmlString("#!table tsv some classes\na\tb"), <table class="InterpreterTable simpleTable some classes"><tbody><tr><td><p>a</p></td><td><p>b</p></td></tr></tbody></table>.toString())
      assertEquals(Interpreters.toHtmlString("#!table tsv 1 some classes\na\tb"), <table class="InterpreterTable simpleTable some classes"><thead><tr><th><p>a</p></th><th><p>b</p></th></tr></thead><tbody></tbody></table>.toString())
      assertEquals(Interpreters.toHtmlString("#!table tsv 1 tablesorter\na\tb"), <table class="InterpreterTable simpleTable tablesorter"><thead><tr><th><p>a</p></th><th><p>b</p></th></tr></thead><tbody></tbody></table>.toString())
      assertEquals(Interpreters.toHtmlString("#!table tsv 0 1 some classes\na\tb"), <table class="InterpreterTable simpleTable some classes"><tbody><tr><th><p>a</p></th><td><p>b</p></td></tr></tbody></table>.toString())
    }; testInterpreterTable()

    def testInterpreterWiki()(implicit request: Request[Any], syncCacheApi: SyncCacheApi): Unit = {
      import models.tables.Link

      assertEquals(InterpreterWiki.name, "Wiki")

      assertEquals(InterpreterWiki.inlineToHtmlString("""http://a.com"""), """<a href="http://a.com" target="_blank">http://a.com</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""http://a.com$"""), """<a href="http://a.com$" target="_blank">http://a.com$</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""[http://a.com]"""), """<a href="http://a.com" target="_blank">http://a.com</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""[http://a.com a com]"""), """<a href="http://a.com" target="_blank">a com</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""[FrontPage]"""), """<a href="FrontPage">FrontPage</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""[FrontPage Alias]"""), """<a href="FrontPage">Alias</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""[wiki:FrontPage]"""), """<a href="FrontPage">FrontPage</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""[wiki:FrontPage Alias]"""), """<a href="FrontPage">Alias</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""["Some Page"]"""), """<a href="Some Page" class="missing">Some Page</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""["Some Page" Alias]"""), """<a href="Some Page" class="missing">Alias</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""[schema:Schema]"""), """<a href="./schema:Schema" class="schema">schema:Schema</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""[schema:Schema Alias]"""), """<a href="./schema:Schema" class="schema">Alias</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""["schema:Schema"]"""), """<a href="./schema:Schema" class="schema">schema:Schema</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""["schema:Schema" Alias]"""), """<a href="./schema:Schema" class="schema">Alias</a>""")

      assertEquals(InterpreterWiki.inlineToHtmlString("""http://a.com/$   [http://a.com]  [http://a.com a com]"""), """<a href="http://a.com/$" target="_blank">http://a.com/$</a>   <a href="http://a.com" target="_blank">http://a.com</a>  <a href="http://a.com" target="_blank">a com</a>""")

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

      assertEquals(InterpreterWiki.inlineToHtmlString("""\\http://a.com"""), """\\<a href="http://a.com" target="_blank">http://a.com</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\\http://a.com$"""), """\\<a href="http://a.com$" target="_blank">http://a.com$</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\\[http://a.com]"""), """\\<a href="http://a.com" target="_blank">http://a.com</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\\[http://a.com a com]"""), """\\<a href="http://a.com" target="_blank">a com</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\\[FrontPage]"""), """\\<a href="FrontPage">FrontPage</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\\[FrontPage Alias]"""), """\\<a href="FrontPage">Alias</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\\[wiki:FrontPage]"""), """\\<a href="FrontPage">FrontPage</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\\[wiki:FrontPage Alias]"""), """\\<a href="FrontPage">Alias</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\\["Some Page"]"""), """\\<a href="Some Page" class="missing">Some Page</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\\["Some Page" Alias]"""), """\\<a href="Some Page" class="missing">Alias</a>""")



      assertEquals(InterpreterWiki.extractLinkMarkup("""http://a.com""").toList, Seq(LinkMarkup("""http://a.com""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""http://a.com$""").toList, Seq(LinkMarkup("""http://a.com$""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""[http://a.com]""").toList, Seq(LinkMarkup("""http://a.com""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""[http://a.com a com]""").toList, Seq(LinkMarkup("""http://a.com""", """a com""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""[FrontPage]""").toList, Seq(LinkMarkup("""FrontPage""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""[FrontPage Alias]""").toList, Seq(LinkMarkup("""FrontPage""", """Alias""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""[wiki:FrontPage]""").toList, Seq(LinkMarkup("""wiki:FrontPage""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""[wiki:FrontPage Alias]""").toList, Seq(LinkMarkup("""wiki:FrontPage""", """Alias""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""["Some Page"]""").toList, Seq(LinkMarkup("""Some Page""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""["Some Page" Alias]""").toList, Seq(LinkMarkup("""Some Page""", "Alias")))

      assertEquals(
        InterpreterWiki.extractLinkMarkup("""http://a.com/$   [http://a.com]  [http://a.com a com]""").toList,
        Seq(LinkMarkup("""http://a.com/$"""), LinkMarkup("""http://a.com"""), LinkMarkup("""http://a.com""", """a com"""))
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

      assertEquals(InterpreterWiki.extractLinkMarkup("""\\http://a.com""").toList, Seq(LinkMarkup("""http://a.com""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""\\http://a.com$""").toList, Seq(LinkMarkup("""http://a.com$""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""\\[http://a.com]""").toList, Seq(LinkMarkup("""http://a.com""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""\\[http://a.com a com]""").toList, Seq(LinkMarkup("""http://a.com""", """a com""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""\\[FrontPage]""").toList, Seq(LinkMarkup("""FrontPage""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""\\[FrontPage Alias]""").toList, Seq(LinkMarkup("""FrontPage""", """Alias""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""\\[wiki:FrontPage]""").toList, Seq(LinkMarkup("""wiki:FrontPage""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""\\[wiki:FrontPage Alias]""").toList, Seq(LinkMarkup("""wiki:FrontPage""", """Alias""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""\\["SomePage"]""").toList, Seq(LinkMarkup("""SomePage""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""\\["SomePage" Alias]""").toList, Seq(LinkMarkup("""SomePage""", """Alias""")))

      assertEquals(InterpreterWiki.toSeqLink("[link]").toList, Seq(Link("UnitTest", "link", "")))
      assertEquals(InterpreterWiki.toSeqLink("[link alias][b]").toList, Seq(Link("UnitTest", "link", "alias"), Link("UnitTest", "b", "")))

    }; testInterpreterWiki()

    def testInterpreterSchema()(implicit request: Request[Any], syncCacheApi: SyncCacheApi): Unit = {
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
            |          <div><a class="schema" href="./schema:Thing">Thing</a> / <a class="schema" href="./schema:Person">Person</a></div>
            |        </h5>
            |        <div>
            |          <div>
            |                <dt>
            |                  <span title="The name of the item." class="">Name </span>
            |                </dt>
            |                <dd property="name"><a href="KIM, Aha" class="missing">KIM, Aha</a></dd>
            |              </div><div>
            |                <dt>
            |                  <span title="URL of the item." class="">Url </span>
            |                </dt>
            |                <dd property="url"><a href="https://aha00a.com" target="_blank">https://aha00a.com</a></dd>
            |              </div><div>
            |                <dt>
            |                  <span title="An Organization (or ProgramMembership) to which this Person or Organization belongs." class="">Member Of </span>
            |                </dt>
            |                <dd property="memberOf"><a href="AhariseNotExists" class="missing">AhariseNotExists</a></dd>
            |              </div>
            |        </div>
            |      </dl></div>""".stripMargin

        assertEquals(InterpreterSchema.toHtmlString(schemaMarkup), interpreted)
        assertEquals(Interpreters.toHtmlString(wikiMarkup), interpreted)

        val extractWordResult = Seq("#!Schema", "Person", "name", "KIM,", "Aha", "url", "https://aha00a.com", "memberOf", "AhariseNotExists")
        assertEquals(InterpreterSchema.toSeqWord(schemaMarkup), extractWordResult)
        assertEquals(Interpreters.toSeqWord(wikiMarkup), extractWordResult)

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
          SchemaOrg("UnitTest", "WebApplication", "datePublished", "----21"),
          SchemaOrg("UnitTest", "WebApplication", "datePublished", "2015"),
          SchemaOrg("UnitTest", "WebApplication", "datePublished", "--10-21"),
          SchemaOrg("UnitTest", "WebApplication", "datePublished", "--10")
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
    Ok("Ok.")
  }


  def gradient: Action[AnyContent] = Action { implicit request =>
    Ok(views.html.Test.gradient(""))
  }



}



