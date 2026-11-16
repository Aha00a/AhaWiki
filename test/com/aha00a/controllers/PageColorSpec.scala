package com.aha00a.controllers

import anorm.SQL
import com.aha00a.tests.TestApplication
import com.aha00a.tests.TestSchema
import logics.PageColor
import logics.wikis.interpreters.InterpreterSchema
import logics.wikis.interpreters.InterpreterSchema.ParseResult
import org.scalatest.BeforeAndAfterAll
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Application
import play.api.cache.SyncCacheApi
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.JsArray
import play.api.test.FakeRequest
import play.api.test.Helpers._

/** A page's own colour, written as `themeColor` in its #!Schema block (logics.PageColor), and the
  * two endpoints that hand it to the browser: /api/links for the adjacent-pages graph and
  * /api/pagePreview for the link preview. Both read the rows the page calculation writes
  * (CalculatedSchemaOrg), so the spec writes those rows directly. A site of its own, because the
  * memory caches are shared by every spec in the JVM and keyed by site. */
class PageColorSpec extends PlaySpec with GuiceOneAppPerSuite with BeforeAndAfterAll {

  private val host = "page-color.test"

  override def fakeApplication(): Application =
    GuiceApplicationBuilder()
      .configure(TestApplication.baseConfiguration(TestApplication.randomDbName("page_color")))
      .overrides(bind[SyncCacheApi].toInstance(new TestApplication.TestSyncCacheApi))
      .build()

  override def beforeAll(): Unit = {
    super.beforeAll()
    app.injector.instanceOf[play.api.db.Database].withConnection { implicit connection =>
      TestSchema.createAll()
      Seq(
        s"INSERT INTO Site (seq, name, abbr, mainDomain) VALUES (63, 'PageColor', 'PageColor', '$host')",
        s"INSERT INTO SiteDomain (site, domain) VALUES (63, '$host')",
        "INSERT INTO Permission (site, target, targetType, actor, actorType, action) VALUES (63, '', 'All', '', 'All', 255)",
        "INSERT INTO `User` (seq, nickname) VALUES (63, 'pagecolor')",
      ).foreach(sql => SQL(sql).execute())
      Seq("Hub", "Red", "Plain", "Broken").foreach { name =>
        SQL("INSERT INTO Page (site, name, revision, dateTime, `user`, remoteAddress, comment, content) VALUES (63, {name}, 1, NOW(), 63, '127.0.0.1', '', {name})")
          .on("name" -> name).execute()
        SQL("INSERT INTO PageMeta (site, name, revision) VALUES (63, {name}, 1)").on("name" -> name).execute()
      }
      Seq("Red", "Plain", "Broken").foreach { name =>
        SQL("INSERT INTO CalculatedLink (site, src, dst, alias) VALUES (63, 'Hub', {name}, '')").on("name" -> name).execute()
      }
      // Hub declares two classes, so the calculation writes its colour once per class.
      Seq(
        ("Hub", "Person", "#0A0"),
        ("Hub", "Thing", "#00aa00"),
        ("Red", "Movie", "#E60012"),
        ("Broken", "Movie", "red; background: url(x)"),
      ).foreach { case (page, cls, value) =>
        SQL("INSERT INTO CalculatedSchemaOrg (site, page, cls, prop, value) VALUES (63, {page}, {cls}, 'themeColor', {value})")
          .on("page" -> page, "cls" -> cls, "value" -> value).execute()
      }
    }
    TestApplication.resetMemoryCaches()
    logics.AhaWikiCacheMemoryDomainSite.refresh()(app.injector.instanceOf[play.api.db.Database])
  }

  override def afterAll(): Unit = {
    TestApplication.resetMemoryCaches()
    super.afterAll()
  }

  "PageColor" should {
    "take #rgb and #rrggbb, lower-cased and long, and nothing else" in {
      PageColor.parse("#E60012") mustBe Some("#e60012")
      PageColor.parse(" #0a0 ") mustBe Some("#00aa00")
      PageColor.parse("red") mustBe None
      PageColor.parse("#e60012; background: url(x)") mustBe None
      PageColor.parse("#e6001") mustBe None
      PageColor.parse("rgb(1,2,3)") mustBe None
    }

    "give the hue a large surface draws with" in {
      PageColor.hue("#ff0000") mustBe 0
      PageColor.hue("#00ff00") mustBe 120
      PageColor.hue("#0000ff") mustBe 240
      PageColor.hue("#808080") mustBe 0
    }

    "take the first valid themeColor of a block" in {
      PageColor.fromFields(Seq(Seq("name", "x"), Seq("themeColor", "nope", "#123"), Seq("themeColor", "#456"))) mustBe Some("#112233")
      PageColor.fromFields(Seq(Seq("name", "x"))) mustBe None
    }
  }

  "A #!Schema block" should {
    "leave themeColor out of the JSON-LD: schema.org has no such property" in {
      val json = InterpreterSchema.toJsonLdObject(ParseResult("Movie", Seq(Seq("name", "x"), Seq("themeColor", "#e60012")))).get
      (json \ "themeColor").toOption mustBe None
      (json \ "name").as[String] mustBe "x"
    }
  }

  "/api/links" should {
    "give each page's colour, and none for a page without a valid one" in {
      val result = route(app, FakeRequest(GET, "/api/links/Hub").withHeaders("Host" -> host)).get
      status(result) mustBe OK
      val links = contentAsJson(result).as[JsArray].value
      links.foreach(link => (link \ "srcColor").as[String] mustBe "#00aa00")
      links.map(link => (link \ "dst").as[String] -> (link \ "dstColor").as[String]).toMap mustBe Map(
        "Red" -> "#e60012",
        "Plain" -> "",
        "Broken" -> "",
      )
    }
  }

  "/api/pagePreview" should {
    "give the page's colour" in {
      (contentAsJson(route(app, FakeRequest(GET, "/api/pagePreview/Red").withHeaders("Host" -> host)).get) \ "color").as[String] mustBe "#e60012"
      (contentAsJson(route(app, FakeRequest(GET, "/api/pagePreview/Plain").withHeaders("Host" -> host)).get) \ "color").as[String] mustBe ""
    }
  }
}
