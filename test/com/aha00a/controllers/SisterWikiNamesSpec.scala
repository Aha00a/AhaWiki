package com.aha00a.controllers

import anorm.SQL
import com.aha00a.tests.TestApplication
import com.aha00a.tests.TestSchema
import org.scalatest.BeforeAndAfterAll
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Application
import play.api.cache.SyncCacheApi
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers._

import java.net.URLEncoder

/** A page's name is chosen by whoever made the page, and two generated lists show names from
  * OTHER wikis of the instance: Similar Pages (a similarity row can join pages of two sites) and
  * Twin Pages (the same name on a sister wiki, for a name that on this wiki may come straight
  * from the address). Both lists were written as AhaMark and rendered, so a name was read as
  * markup -- and `[[Html(...)]]` is markup. An author of one wiki could run HTML for the readers
  * of another. Raw HTML is an author's to write on their own wiki; this handed it across.
  *
  * Two sites of their own seq and host, for the reason AccessControlFilteringSpec gives.
  */
class SisterWikiNamesSpec extends PlaySpec with GuiceOneAppPerSuite with BeforeAndAfterAll {

  private val dbName = TestApplication.randomDbName("sister_names")
  private val here = "names-here.test"

  // The first closes the macro's argument and opens another macro; the second closes a link.
  private val nameThatLeavesAMacro = "a)]][[Html(<img id=injectedBySimilar src=x>)]]"
  private val nameThatLeavesALink = "x] [[Html(<img id=injectedByTwin src=x>)]]"

  override def fakeApplication(): Application = {
    GuiceApplicationBuilder()
      .configure(TestApplication.baseConfiguration(dbName))
      .overrides(bind[SyncCacheApi].toInstance(new TestApplication.TestSyncCacheApi))
      .build()
  }

  override def beforeAll(): Unit = {
    super.beforeAll()
    app.injector.instanceOf[play.api.db.Database].withConnection { implicit connection =>
      TestSchema.createAll()
      Seq(
        "INSERT INTO Site (seq, name, abbr, mainDomain) VALUES (11, 'HereWiki', 'Here', 'names-here.test')",
        "INSERT INTO Site (seq, name, abbr, mainDomain) VALUES (12, 'ThereWiki', 'There', 'names-there.test')",
        "INSERT INTO SiteDomain (site, domain) VALUES (11, 'names-here.test')",
        "INSERT INTO SiteDomain (site, domain) VALUES (12, 'names-there.test')",
        "INSERT INTO `User` (seq, nickname) VALUES (31, 'writer')",
        "INSERT INTO Permission (site, target, targetType, actor, actorType, action) VALUES (11, '', 'All', '', 'All', 1)",
        "INSERT INTO Permission (site, target, targetType, actor, actorType, action) VALUES (12, '', 'All', '', 'All', 1)",
      ).foreach(sql => SQL(sql).execute())

      def page(site: Int, name: String): Unit = {
        SQL("INSERT INTO Page (site, name, revision, dateTime, `user`, remoteAddress, comment, content) VALUES ({site}, {name}, 1, NOW(), 31, '127.0.0.1', '', 'text')")
          .on("site" -> site, "name" -> name).execute()
        SQL("INSERT INTO PageMeta (site, name, revision) VALUES ({site}, {name}, 1)")
          .on("site" -> site, "name" -> name).execute()
      }
      def similar(name1: String, site2: Int, name2: String, similarity: Double): Unit =
        SQL("INSERT INTO CalculatedCosineSimilarity (site1, name1, site2, name2, similarity) VALUES (11, {name1}, {site2}, {name2}, {similarity})")
          .on("name1" -> name1, "site2" -> site2, "name2" -> name2, "similarity" -> similarity).execute()

      Seq("Subject", "Calm", "Both").foreach(page(11, _))
      Seq("Plain", nameThatLeavesAMacro, nameThatLeavesALink).foreach(page(12, _))
      similar("Subject", 12, "Plain", 0.7)
      similar("Subject", 12, nameThatLeavesAMacro, 0.6)
      similar("Calm", 12, "Plain", 0.7)
      similar("Both", 11, "Calm", 0.8)
      similar("Both", 12, "Plain", 0.7)
    }
    TestApplication.resetMemoryCaches()
  }

  override def afterAll(): Unit = {
    TestApplication.resetMemoryCaches()
    super.afterAll()
  }

  /** What an anonymous reader of HereWiki gets for `markup` drawn as the page `pageName`. */
  private def rendered(pageName: String, markup: String): String = {
    val path = "/api/renderAhaMark/" + URLEncoder.encode(pageName, "UTF-8").replace("+", "%20")
    val result = route(app, FakeRequest(POST, path).withHeaders(HOST -> here).withJsonBody(Json.obj("comment" -> markup))).get
    status(result) mustBe OK
    (contentAsJson(result) \ "html").as[String]
  }

  private def theList(html: String): String =
    """(?s)<ul.*</ul>""".r.findFirstIn(html).getOrElse(fail(s"no list in: $html"))

  "[[SimilarPages]]" should {
    "show a sister wiki's page name as text, whatever the name holds" in {
      val html = rendered("Subject", "[[SimilarPages]]")
      html must include("There:Plain")
      html must not include "<img id=injectedBySimilar"
      // The whole name, as its author typed it, shown and not run.
      html must include("There:a)]][[Html(&lt;img id=injectedBySimilar src=x&gt;)]]")
      html must not include "Argument Error"
    }

    // The fix draws each line from values instead of writing it as macros. For a name with
    // nothing in it to read as markup the page must come out as it always did: the same list,
    // made by the renderer, around the same two macros' HTML.
    "draw a plain name exactly as the macros it used to write would" in {
      theList(rendered("Calm", "[[SimilarPages]]")) mustBe
        theList(rendered("Calm", """ 1. [[PercentLinkTitle(0.7, There:Plain, "")]] [[Trivial()]]"""))
    }

    "draw the two groups exactly as before too" in {
      theList(rendered("Both", "[[SimilarPages]]")) mustBe theList(rendered("Both",
        """ * Same Wiki
          |  1. [[PercentLinkTitle(0.8, Calm, "")]] [[Trivial()]]
          | * Sister Wikis
          |  1. [[PercentLinkTitle(0.7, There:Plain, "")]] [[Trivial()]]""".stripMargin))
    }
  }

  "[[TwinPages]]" should {
    // The page does not exist here, so its name is whatever the address says: a link someone
    // made is enough to draw this list under a name of their choosing.
    "show the name as text, whatever the name holds" in {
      val html = rendered(nameThatLeavesALink, "[[TwinPages]]")
      html must not include "<img id=injectedByTwin"
      html must include("There:x] [[Html(&lt;img id=injectedByTwin src=x&gt;)]]")
    }

    "draw a plain name exactly as the link it used to write would" in {
      theList(rendered("Plain", "[[TwinPages]]")) mustBe
        theList(rendered("Plain", " 1. [https://names-there.test/w/Plain There:Plain]"))
    }
  }

  "a macro's argument error" should {
    // The box quotes the call back. It is HTML, so what it quotes is escaped: an argument is
    // not always the page author's own text -- the two lists above passed names through here.
    "quote the argument as text" in {
      val html = rendered("Subject", "[[Sparkline(1 <img id=injectedByError src=x>)]]")
      html must include("Argument Error - [[Sparkline(1 &lt;img id=injectedByError src=x&gt;)]]")
      html must not include "<img id=injectedByError"
    }
  }
}
