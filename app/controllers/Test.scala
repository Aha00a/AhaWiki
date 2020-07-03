package controllers

import akka.actor.ActorRef
import akka.actor.ActorSystem
import anorm.SQL
import anorm.SqlParser.long
import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.EnglishCaseConverter
import javax.inject.Inject
import logics.wikis.HeadingNumber
import logics.wikis.interpreters.InterpreterVim
import logics.wikis.interpreters.InterpreterWiki.LinkMarkup
import logics.wikis.interpreters.InterpreterSchema
import logics.wikis.interpreters.InterpreterWiki
import logics.wikis.interpreters.Interpreters
import logics.wikis.macros._
import models._
import play.api.cache.SyncCacheApi
import play.api.db.Database
import play.api.mvc._
import play.api.Configuration
import play.api.Logger
import play.api.Logging

class Test @Inject()(implicit val
                     controllerComponents: ControllerComponents,
                     syncCacheApi: SyncCacheApi,
                     system: ActorSystem,
                     database: play.api.db.Database,
                     @javax.inject.Named("db-actor") actorAhaWiki: ActorRef,
                     configuration: Configuration
                    ) extends BaseController with Logging {

  case class ExceptionEquals[T](actual: T, expect: T) extends Exception(s"\nActual=($actual)\nExpect=($expect)") {
    logger.error(actual.toString)
    logger.error(expect.toString)
  }


  def assertEquals[T](actual: T, expect: T): Unit = {
    if (actual == expect) {

    } else {
      throw ExceptionEquals(actual, expect)
    }
  }

  def assertEquals(actual: String, expect: String): Unit = {
    if (actual == expect) {

    } else if (actual == expect.replace("\r", "")) {

    } else {
      throw ExceptionEquals(actual, expect)
    }
  }

  def assertEquals[T](actual: Seq[T], expect: Seq[T]): Unit = {
    if (actual.isEmpty && expect.isEmpty) {

    }
    else {
      if (actual == expect) {

      } else {
        throw ExceptionEquals(actual, expect)
      }
    }
  }


  def unit: Action[AnyContent] = Action { implicit request =>
    assertEquals("aa".toIntOrZero, 0)
    assertEquals("10".toIntOrZero, 10)

    testEnglishCaseConverter()

    implicit val wikiContext: WikiContext = WikiContext("UnitTest")

    testPageContent()

    testTraitMacroName()

    testMacroBr()
    testMacroMonths()

    testHeadingNumber()

    testInterpreterVim()
    testInterpreterTable()
    testInterpreterSchema()
    testInterpreterWiki()


    testBlame()
    testParboiled()

    Ok("Ok.")
  }

  def testMacroMonths()(implicit wikiContext: WikiContext): Unit = {
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
  }

  def testTraitMacroName(): Unit = {
    assertEquals(MacroMonths.name, "Months")
    assertEquals(MacroCalendar.name, "Calendar")
    assertEquals(MacroWeekdayName.name, "WeekdayName")
  }

  def testPageContent(): Unit = {
    {
      val pageContent: PageContent = PageContent(
        """#!read all
          |#!write aha00a
          |#!redirect FrontPage""".stripMargin)
      assertEquals(pageContent.read, Some("all"))
      assertEquals(pageContent.write, Some("aha00a"))
      assertEquals(pageContent.redirect, Some("FrontPage"))
      assertEquals(pageContent.shebang, Array[String]().toSeq)
      assertEquals(pageContent.content, "")
    }
    {
      val pageContent: PageContent = PageContent(
        """#!read login
          |#!write login
          |something""".stripMargin)
      assertEquals(pageContent.read, Some("login"))
      assertEquals(pageContent.write, Some("login"))
      assertEquals(pageContent.redirect, None)
      assertEquals(pageContent.shebang, List())
      assertEquals(pageContent.interpreter, None)
      assertEquals(pageContent.content, "something")
    }
    {
      val pageContent: PageContent = PageContent(
        """#!Wiki
          |#!Html
          |something""".stripMargin)
      assertEquals(pageContent.read, None)
      assertEquals(pageContent.write, None)
      assertEquals(pageContent.redirect, None)
      assertEquals(pageContent.shebang, List("Wiki", "Html"))
      assertEquals(pageContent.content, "something")
    }
    {
      val pageContent: PageContent = PageContent(
        """#!Paper a b
          |#!read login
          |#!write login
          |something""".stripMargin)
      assertEquals(pageContent.read, Some("login"))
      assertEquals(pageContent.write, Some("login"))
      assertEquals(pageContent.redirect, None)
      assertEquals(pageContent.interpreter, Some("Paper"))
      assertEquals(pageContent.content, "something")
    }
    {
      val pageContent: PageContent = PageContent(
        """#!read login
          |#!write login
          |#!Paper a b
          |something""".stripMargin)
      assertEquals(pageContent.read, Some("login"))
      assertEquals(pageContent.write, Some("login"))
      assertEquals(pageContent.redirect, None)
      assertEquals(pageContent.interpreter, Some("Paper"))
      assertEquals(pageContent.content, "something")
    }
    {
      val pageContent: PageContent = PageContent(
        """#!WikiSyntaxPreview Text
          |some text
          |""".stripMargin)
      assertEquals(pageContent.read, None)
      assertEquals(pageContent.write, None)
      assertEquals(pageContent.redirect, None)
      assertEquals(pageContent.shebang, List("WikiSyntaxPreview", "Text"))
      assertEquals(pageContent.content, "some text")
    }
    {
      val pageContent: PageContent = PageContent(
        """#!WikiSyntaxPreview Html
          |<h1>h1</h1>
          |<p>paragraph</p>
          |""".stripMargin)
      assertEquals(pageContent.read, None)
      assertEquals(pageContent.write, None)
      assertEquals(pageContent.redirect, None)
      assertEquals(pageContent.shebang, List("WikiSyntaxPreview", "Html"))
      assertEquals(pageContent.content, "<h1>h1</h1>\n<p>paragraph</p>")
    }
    {
      val pageContent: PageContent = PageContent(
        """#!WikiSyntaxPreview Vim
          |#!java
          |class C {
          |    private String s = "Hello, java!";
          |}
          |""".stripMargin)
      assertEquals(pageContent.read, None)
      assertEquals(pageContent.write, None)
      assertEquals(pageContent.redirect, None)
      assertEquals(pageContent.shebang, List("WikiSyntaxPreview", "Vim", "java"))
      assertEquals(pageContent.content, "class C {\n    private String s = \"Hello, java!\";\n}")
    }
    {
      assertEquals("a:b:::c:".split(":", -1).toList, List("a", "b", "", "", "c", ""))
      val pageContent: PageContent = PageContent(
        """aaa
          |[[[#!Vim java
          |a
          |
          |b
          |
          |c
          |]]]
          |""".stripMargin)
      assertEquals(pageContent.read, None)
      assertEquals(pageContent.write, None)
      assertEquals(pageContent.redirect, None)
      assertEquals(pageContent.shebang, List[String]())
      assertEquals(pageContent.content, "aaa\n[[[#!Vim java\na\n\nb\n\nc\n]]]")
    }

  }

  def testInterpreterTable()(implicit request: Request[Any], syncCacheApi: SyncCacheApi): Unit = {
    implicit val wikiContext: WikiContext = WikiContext("UnitTest")

    assertEquals(Interpreters.toHtmlString("#!table tsv\na\tb"), <table class="InterpreterTable simpleTable"><tbody><tr><td><p>a</p></td><td><p>b</p></td></tr></tbody></table>.toString())
    assertEquals(Interpreters.toHtmlString("#!table\n#!tsv\na\tb"), <table class="InterpreterTable simpleTable"><tbody><tr><td><p>a</p></td><td><p>b</p></td></tr></tbody></table>.toString())
    assertEquals(Interpreters.toHtmlString("#!table tsv 1\na\tb"), <table class="InterpreterTable simpleTable"><thead><tr><th><p>a</p></th><th><p>b</p></th></tr></thead><tbody></tbody></table>.toString())
    assertEquals(Interpreters.toHtmlString("#!table tsv 0 1\na\tb"), <table class="InterpreterTable simpleTable"><tbody><tr><th><p>a</p></th><td><p>b</p></td></tr></tbody></table>.toString())

    assertEquals(Interpreters.toHtmlString("#!table tsv some classes\na\tb"), <table class="InterpreterTable simpleTable some classes"><tbody><tr><td><p>a</p></td><td><p>b</p></td></tr></tbody></table>.toString())
    assertEquals(Interpreters.toHtmlString("#!table tsv 1 some classes\na\tb"), <table class="InterpreterTable simpleTable some classes"><thead><tr><th><p>a</p></th><th><p>b</p></th></tr></thead><tbody></tbody></table>.toString())
    assertEquals(Interpreters.toHtmlString("#!table tsv 1 tablesorter\na\tb"), <table class="InterpreterTable simpleTable tablesorter"><thead><tr><th><p>a</p></th><th><p>b</p></th></tr></thead><tbody></tbody></table>.toString())
    assertEquals(Interpreters.toHtmlString("#!table tsv 0 1 some classes\na\tb"), <table class="InterpreterTable simpleTable some classes"><tbody><tr><th><p>a</p></th><td><p>b</p></td></tr></tbody></table>.toString())
  }

  def testInterpreterWiki()(implicit request: Request[Any], syncCacheApi: SyncCacheApi): Unit = {
    import models.tables.Link
    implicit val wikiContext: WikiContext = WikiContext("UnitTest")

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

  }

  def testInterpreterSchema()(implicit request: Request[Any], syncCacheApi: SyncCacheApi): Unit = {
    import models.tables.SchemaOrg
    implicit val wikiContext: WikiContext = WikiContext("UnitTest")

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
  }



  //noinspection NameBooleanParameters
  def testInterpreterVim(): Unit = {
    def test(p: InterpreterVim.Parser, syntax: String, content: String, isError: Boolean): Unit = {
      assertEquals(p.syntax, syntax)
      assertEquals(p.content, content)
      assertEquals(p.isError, isError)
    }

    test(InterpreterVim.Parser(""), "", "", true)
    test(InterpreterVim.Parser("#!Vi"), "", "", true)
    test(InterpreterVim.Parser("#!Vim"), "", "", false)
    test(InterpreterVim.Parser("#!Vim c"), "c", "", false)
    test(InterpreterVim.Parser("#!Vim cpp"), "cpp", "", false)
    test(InterpreterVim.Parser("#!Vim\n"), "", "", false)
    test(InterpreterVim.Parser("#!Vim cpp\n"), "cpp", "", false)
    test(InterpreterVim.Parser("#!Vim cpp\nasdf"), "cpp", "asdf", false)
    test(InterpreterVim.Parser("#!Vim\n#!cpp\nasdf"), "cpp", "asdf", false)
    test(InterpreterVim.Parser("#!Vim cpp\nasdf\nasdf"), "cpp", "asdf\nasdf", false)
    test(InterpreterVim.Parser("#!Vim\n#!cpp\nasdf\nasdf"), "cpp", "asdf\nasdf", false)
    test(InterpreterVim.Parser("#!Vim\n#!sh\n#!/bin/sh\nasdf"), "sh", "#!/bin/sh\nasdf", false)
    test(InterpreterVim.Parser("#!Vim\n#!sh\n#!/bin/sh\nasdf\na\n\nb\n\nc"), "sh", "#!/bin/sh\nasdf\na\n\nb\n\nc", false)
  }

  def testHeadingNumber(): Unit = {
    val headingNumber: HeadingNumber = new HeadingNumber()
    assertEquals(headingNumber.incrGet(1), "1.")
    assertEquals(headingNumber.incrGet(1), "2.")
    assertEquals(headingNumber.incrGet(1), "3.")
    assertEquals(headingNumber.incrGet(1), "4.")
    assertEquals(headingNumber.incrGet(2), "4.1.")
    assertEquals(headingNumber.incrGet(2), "4.2.")
    assertEquals(headingNumber.incrGet(2), "4.3.")
    assertEquals(headingNumber.incrGet(1), "5.")
    assertEquals(headingNumber.incrGet(2), "5.1.")
    assertEquals(headingNumber.incrGet(2), "5.2.")
    assertEquals(headingNumber.incrGet(2), "5.3.")
    assertEquals(headingNumber.incrGet(3), "5.3.1.")
    assertEquals(headingNumber.incrGet(3), "5.3.2.")
    assertEquals(headingNumber.incrGet(3), "5.3.3.")
    assertEquals(headingNumber.incrGet(4), "5.3.3.1.")
    assertEquals(headingNumber.incrGet(4), "5.3.3.2.")
    assertEquals(headingNumber.incrGet(4), "5.3.3.3.")
    assertEquals(headingNumber.incrGet(1), "6.")
    assertEquals(headingNumber.incrGet(2), "6.1.")
    assertEquals(headingNumber.incrGet(2), "6.2.")
    assertEquals(headingNumber.incrGet(2), "6.3.")
    assertEquals(headingNumber.incrGet(3), "6.3.1.")
    assertEquals(headingNumber.incrGet(3), "6.3.2.")
    assertEquals(headingNumber.incrGet(3), "6.3.3.")
    assertEquals(headingNumber.incrGet(4), "6.3.3.1.")
    assertEquals(headingNumber.incrGet(4), "6.3.3.2.")
    assertEquals(headingNumber.incrGet(4), "6.3.3.3.")
  }

  def testMacroBr()(implicit wikiContext: WikiContext): Unit = {
    val empty = ""
    val dummy = "aaaa"

    assertEquals(MacroBr.toHtmlString(empty), "<br/>")
    assertEquals(MacroBr.toHtmlString(dummy), "<br/>")
    assertEquals(MacroBr.extractLink(empty), Seq())
    assertEquals(MacroBr.extractLink(dummy), Seq())
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

  def testBlame(): Unit = {
    testBlame1()
    testBlame2()
  }

  //noinspection ZeroIndexToHead
  def testBlame1(): Unit = {
    class MetaData(val revision: Int)
    assertEquals(new Blame().size, 0)

    val blame1 = new Blame().next(new MetaData(1), "A".splitLinesSeq())
    assertEquals(blame1.size, 1)
    assertEquals(blame1.seqBlameLine(0).metaData.revision, 1)
    assertEquals(blame1.seqBlameLine(0).item, "A")

    val blame2 = blame1.next(new MetaData(2), "B".splitLinesSeq())
    assertEquals(blame2.size, 1)
    assertEquals(blame2.seqBlameLine(0).metaData.revision, 2)
    assertEquals(blame2.seqBlameLine(0).item, "B")

    val blame3 = blame2.next(new MetaData(3), "a\nb\nc\nd\ne".splitLinesSeq())
    assertEquals(blame3.size, 5)
    assertEquals(blame3.seqBlameLine(0).metaData.revision, 3)
    assertEquals(blame3.seqBlameLine(0).item, "a")
    assertEquals(blame3.seqBlameLine(1).metaData.revision, 3)
    assertEquals(blame3.seqBlameLine(1).item, "b")
    assertEquals(blame3.seqBlameLine(2).metaData.revision, 3)
    assertEquals(blame3.seqBlameLine(2).item, "c")
    assertEquals(blame3.seqBlameLine(3).metaData.revision, 3)
    assertEquals(blame3.seqBlameLine(3).item, "d")
    assertEquals(blame3.seqBlameLine(4).metaData.revision, 3)
    assertEquals(blame3.seqBlameLine(4).item, "e")

    val blame4 = blame3.next(new MetaData(4), "a\nb\nd\ne".splitLinesSeq())
    assertEquals(blame4.size, 4)
    assertEquals(blame4.seqBlameLine(0).metaData.revision, 3)
    assertEquals(blame4.seqBlameLine(0).item, "a")
    assertEquals(blame4.seqBlameLine(1).metaData.revision, 3)
    assertEquals(blame4.seqBlameLine(1).item, "b")
    assertEquals(blame4.seqBlameLine(2).metaData.revision, 3)
    assertEquals(blame4.seqBlameLine(2).item, "d")
    assertEquals(blame4.seqBlameLine(3).metaData.revision, 3)
    assertEquals(blame4.seqBlameLine(3).item, "e")
  }

  //noinspection ZeroIndexToHead
  def testBlame2(): Unit = {
    class MetaData(val revision: Int)
    assertEquals(new Blame().size, 0)

    val blame1 = new Blame().next(new MetaData(1), "1\n1\n1\n2\n2\n2\n2\n1\n1\n1".splitLinesSeq())
    val blame2 = blame1.next(new MetaData(2), "1\n1\n2\n2\n1\n1".splitLinesSeq())
    assertEquals(blame2.size, 6)
    assertEquals(blame2.seqBlameLine(0).metaData.revision, 1)
    assertEquals(blame2.seqBlameLine(0).item, "1")
    assertEquals(blame2.seqBlameLine(1).metaData.revision, 1)
    assertEquals(blame2.seqBlameLine(1).item, "1")
    assertEquals(blame2.seqBlameLine(2).metaData.revision, 1)
    assertEquals(blame2.seqBlameLine(2).item, "2")
    assertEquals(blame2.seqBlameLine(3).metaData.revision, 1)
    assertEquals(blame2.seqBlameLine(3).item, "2")
    assertEquals(blame2.seqBlameLine(4).metaData.revision, 1)
    assertEquals(blame2.seqBlameLine(4).item, "1")
    assertEquals(blame2.seqBlameLine(5).metaData.revision, 1)
    assertEquals(blame2.seqBlameLine(5).item, "1")
  }


  def testEnglishCaseConverter(): Unit = {
    assertEquals(EnglishCaseConverter.splitPascalCase("Person"), Seq("Person"))
    assertEquals(EnglishCaseConverter.splitPascalCase("FrontPage"), Seq("Front", "Page"))


    assertEquals(EnglishCaseConverter.camelCase2TitleCase("someWordsAreHere"), "Some Words Are Here")
    assertEquals(EnglishCaseConverter.pascalCase2TitleCase("FrontPage"), "Front Page")
    assertEquals(EnglishCaseConverter.pascalCase2TitleCase("TVSeries"), "TV Series")
  }

  def testParboiled() = {
    import org.parboiled2._

    import scala.util.Try

    class Calculator(val input: ParserInput) extends Parser {
      def InputLine = rule { Expression ~ EOI }

      def Expression: Rule1[Int] = rule {
        Term ~ zeroOrMore(
          '+' ~ Term ~> ((_: Int) + _)
            | '-' ~ Term ~> ((_: Int) - _))
      }

      def Term = rule {
        Factor ~ zeroOrMore(
          '*' ~ Factor ~> ((_: Int) * _)
            | '/' ~ Factor ~> ((_: Int) / _))
      }

      def Factor = rule { Number | Parens }

      def Parens = rule { '(' ~ Expression ~ ')' }

      def Number = rule { capture(Digits) ~> (_.toInt) }

      def Digits = rule { oneOrMore(CharPredicate.Digit) }
    }

    val triedInt: Try[Int] = new Calculator("1+1").InputLine.run()
    assertEquals(triedInt.isSuccess, true)
    assertEquals(triedInt.get, 2)
  }
}



