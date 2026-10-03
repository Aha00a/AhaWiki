package com.aha00a.controllers

import anorm.SQL
import com.aha00a.tests.TestApplication
import com.aha00a.tests.TestSchema
import logics.HtmlJson
import logics.wikis.PageNameUrl
import org.jsoup.Jsoup
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

import scala.jdk.CollectionConverters._

/** Names that reach a script or a JSON attribute as text, with the characters that used to break
  * them: an apostrophe, a double quote, a backslash.
  *
  * Templates wrote such names as '@name' into scripts and by hand into JSON attributes. Twirl's
  * HTML escaping does not make a JavaScript string or JSON: a backslash ended the string early, a
  * newline was a syntax error, and an apostrophe arrived as the text "&#x27;". aha00a.com's
  * PageMap stopped with a SyntaxError on 2026-10-03 over a "page name" holding newlines. The values
  * now go through logics.HtmlJson.
  *
  * The site has a seq and host of its own, because the memory caches are shared by every spec in
  * the JVM and keyed by site. */
class ScriptLiteralSpec extends PlaySpec with GuiceOneAppPerSuite with BeforeAndAfterAll {

  private val host = "script-literal.test"
  private val trickyName = """It's \ "tricky""""
  private val trickyPlace = """Tom's \ Diner"""

  override def fakeApplication(): Application =
    GuiceApplicationBuilder()
      .configure(TestApplication.baseConfiguration(TestApplication.randomDbName("script_literal")) ++ Map(
        // A #!Map block draws an error box without them. The address is in GeocodeCache, so nothing
        // asks Google for it.
        "AhaWiki.google.credentials.api.Geocoding.key" -> "spec-only",
        "AhaWiki.google.credentials.api.MapsJavaScriptAPI.key" -> "spec-only",
      ))
      .overrides(bind[SyncCacheApi].toInstance(new TestApplication.TestSyncCacheApi))
      .build()

  private def insertPage(name: String, content: String)(implicit connection: java.sql.Connection): Unit = {
    SQL("INSERT INTO Page (site, name, revision, dateTime, `user`, remoteAddress, comment, content) VALUES (81, {name}, 1, NOW(), 81, '127.0.0.1', '', {content})")
      .on("name" -> name, "content" -> content).execute()
    SQL("INSERT INTO PageMeta (site, name, revision) VALUES (81, {name}, 1)").on("name" -> name).execute()
  }

  override def beforeAll(): Unit = {
    super.beforeAll()
    app.injector.instanceOf[play.api.db.Database].withConnection { implicit connection =>
      TestSchema.createAll()
      Seq(
        "INSERT INTO Site (seq, name, abbr, mainDomain) VALUES (81, 'ScriptLiteral', 'ScriptLiteral', 'script-literal.test')",
        "INSERT INTO SiteDomain (site, domain) VALUES (81, 'script-literal.test')",
        "INSERT INTO Permission (site, target, targetType, actor, actorType, action) VALUES (81, '', 'All', '', 'All', 255)",
        "INSERT INTO `User` (seq, nickname) VALUES (81, 'writer')",
        "INSERT INTO GeocodeCache (address, lat, lng) VALUES ('Somewhere 1', 37.55, 126.92)",
      ).foreach(sql => SQL(sql).execute())
      insertPage(trickyName, "text")
      insertPage("Graph", Seq("[[[#!Graph", s"$trickyName->plain", "]]]").mkString("\n"))
      insertPage("Places", Seq(
        "#!Map",
        Seq("Name", "Address", "Category", "Comment", "Score").mkString("\t"),
        Seq(trickyPlace, "Somewhere 1", "Place", "", "10").mkString("\t"),
      ).mkString("\n"))
    }
    TestApplication.resetMemoryCaches()
  }

  override def afterAll(): Unit = {
    TestApplication.resetMemoryCaches()
    super.afterAll()
  }

  private def page(path: String): String = {
    val result = route(app, FakeRequest(GET, path).withHeaders(HOST -> host)).get
    status(result) mustBe OK
    contentAsString(result)
  }

  "a page name in a script" should {
    "reach the history page's delete call as a JavaScript string of exactly that name" in {
      page(s"/w/${PageNameUrl.encode(trickyName)}?action=history") must include(s"name: ${HtmlJson.string(trickyName)}")
    }

    "reach a graph's data as a JavaScript string of exactly that name" in {
      val html = page("/w/Graph")
      html must include(HtmlJson.string(trickyName))
      html must include(s"rootNodeName: ${HtmlJson.string("Graph")}")
    }
  }

  "a place name in the map's marker attributes" should {
    "come back exactly when the attribute is decoded and parsed as JSON, as the page's script does" in {
      val row = Jsoup.parse(page("/w/Places")).select("tr[data-label]").asScala.head
      (Json.parse(row.attr("data-label")) \ "text").as[String] mustBe trickyPlace
      Json.parse(row.attr("data-title")).as[String] mustBe s"$trickyPlace - Somewhere 1"
    }
  }
}
