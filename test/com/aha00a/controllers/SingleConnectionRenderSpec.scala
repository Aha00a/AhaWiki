package com.aha00a.controllers

import anorm.SQL
import com.aha00a.tests.TestApplication
import com.aha00a.tests.TestSchema
import logics.SessionLogic
import org.scalatest.BeforeAndAfterAll
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Application
import play.api.cache.SyncCacheApi
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import play.api.test.Helpers._

/** A page view has to get by on the one connection it already holds.
  *
  * The view opens a connection and renders inside it. Macros used to open a second one for
  * themselves. With a pool of ten that is invisible until ten views are being drawn at once: each
  * holds one connection and waits for another that nobody can give back. Every macro then waited
  * out the pool's timeout and failed, pages took 20 to 47 seconds, and every other request got a
  * 500 -- three times in the week before 2026-10-03, each when one crawler opened many pages
  * together.
  *
  * A pool of one makes that deterministic: any second connection asked for while the first is
  * held can never arrive. The timeout is a second rather than Hikari's minimum so that waiting for
  * another thread's short use of the connection (the page calculation a view sometimes queues)
  * still succeeds; waiting for oneself never does.
  *
  * The site has a seq and host of its own, because the memory caches are shared by every spec in
  * the JVM and keyed by site. */
class SingleConnectionRenderSpec extends PlaySpec with GuiceOneAppPerSuite with BeforeAndAfterAll {

  private val dbName = TestApplication.randomDbName("single_connection")
  private val host = "single-connection.test"

  override def fakeApplication(): Application =
    GuiceApplicationBuilder()
      .configure(TestApplication.baseConfiguration(dbName) ++ Map(
        "db.default.hikaricp.maximumPoolSize" -> 1,
        "db.default.hikaricp.minimumIdle" -> 1,
        "db.default.hikaricp.connectionTimeout" -> 1000,
        // Without them a #!Map block draws an error box instead of the map. The address is in
        // GeocodeCache below, so nothing asks Google for it -- an address missing there would.
        "AhaWiki.google.credentials.api.Geocoding.key" -> "spec-only",
        "AhaWiki.google.credentials.api.MapsJavaScriptAPI.key" -> "spec-only",
      ))
      .overrides(bind[SyncCacheApi].toInstance(new TestApplication.TestSyncCacheApi))
      .build()

  private val pageContent =
    """= Pool
      |pool-marker
      |
      |[[Backlinks]]
      |
      |[[SimilarPages]]
      |
      |[[TwinPages]]
      |
      |[[Include(Other)]]
      |
      |[[Years]]
      |
      |[[PageMap]]
      |
      |[[AhaWikiSiteList]]
      |
      |[User:writer]
      |
      |[[[#!Schema
      |Person
      |name	Writer
      |]]]
      |""".stripMargin

  private val mapContent = Seq(
    "#!Map",
    Seq("Name", "Address", "Category", "Comment", "Score").mkString("\t"),
    Seq("Pool", "Somewhere 1", "Place", "map-marker", "10").mkString("\t"),
  ).mkString("\n")

  override def beforeAll(): Unit = {
    super.beforeAll()
    app.injector.instanceOf[play.api.db.Database].withConnection { implicit connection =>
      TestSchema.createAll()
      Seq(
        "INSERT INTO Site (seq, name, abbr, mainDomain) VALUES (61, 'SingleConnection', 'SingleConnection', 'single-connection.test')",
        "INSERT INTO SiteDomain (site, domain) VALUES (61, 'single-connection.test')",
        "INSERT INTO Permission (site, target, targetType, actor, actorType, action) VALUES (61, '', 'All', '', 'All', 255)",
        "INSERT INTO `User` (seq, nickname) VALUES (61, 'writer')",
        "INSERT INTO Page (site, name, revision, dateTime, `user`, remoteAddress, comment, content) VALUES (61, 'Other', 1, NOW(), 61, '127.0.0.1', '', 'other-marker links to [Pool]')",
        "INSERT INTO PageMeta (site, name, revision) VALUES (61, 'Other', 1)",
      ).foreach(sql => SQL(sql).execute())
      SQL("INSERT INTO Page (site, name, revision, dateTime, `user`, remoteAddress, comment, content) VALUES (61, 'Pool', 1, NOW(), 61, '127.0.0.1', '', {content})")
        .on("content" -> pageContent).execute()
      SQL("INSERT INTO Page (site, name, revision, dateTime, `user`, remoteAddress, comment, content) VALUES (61, 'Pool', 2, NOW(), 61, '127.0.0.1', '', {content})")
        .on("content" -> s"${pageContent}second revision").execute()
      SQL("INSERT INTO PageMeta (site, name, revision) VALUES (61, 'Pool', 2)").execute()
      // A map page: InterpreterMap reads geocodes and link counts while it draws.
      SQL("INSERT INTO Page (site, name, revision, dateTime, `user`, remoteAddress, comment, content) VALUES (61, 'Places', 1, NOW(), 61, '127.0.0.1', '', {content})")
        .on("content" -> mapContent).execute()
      SQL("INSERT INTO PageMeta (site, name, revision) VALUES (61, 'Places', 1)").execute()
      SQL("INSERT INTO GeocodeCache (address, lat, lng) VALUES ('Somewhere 1', 37.55, 126.92)").execute()
    }
    TestApplication.resetMemoryCaches()
    // In production the access-log filter resolves the site before the action opens its
    // connection, so the view never loads this cache itself. Specs run without filters.
    logics.AhaWikiCacheMemoryDomainSite.refresh()(app.injector.instanceOf[play.api.db.Database])
  }

  override def afterAll(): Unit = {
    TestApplication.resetMemoryCaches()
    super.afterAll()
  }

  // What a macro or block that could not get a connection leaves in the page.
  private def mustNotHaveStarved(html: String): Unit = {
    html must not include "failed - "
    html must not include "Connection is not available"
  }

  private def mustRenderWhole(html: String): Unit = {
    html must include("pool-marker")
    html must include("other-marker")
    mustNotHaveStarved(html)
  }

  private def get(path: String, loggedIn: Boolean) = {
    val request = FakeRequest(GET, path).withHeaders(HOST -> host)
    val withReader = if (loggedIn) request.withSession(SessionLogic.sessionKeySeq -> "61", SessionLogic.sessionKeyNickname -> "writer") else request
    // The edit, rename and delete screens draw a form, and a form wants the token the CSRF filter
    // would have added. Specs run without filters.
    route(app, withReader.withCSRFToken).get
  }

  "a page view" should {
    "render its macros, its include and its caches without asking the pool for a second connection" in {
      val result = get("/w/Pool", loggedIn = false)
      status(result) mustBe OK
      mustRenderWhole(contentAsString(result))
    }

    "do the same for a reader who is logged in" in {
      val result = get("/w/Pool", loggedIn = true)
      status(result) mustBe OK
      mustRenderWhole(contentAsString(result))
    }

    // Crawlers ask for these far more than readers do, and the 2026-09-28 burst had the edit
    // screen among its pool timeouts. Every one of them goes through Wiki.view.
    "draw a map page on the one connection too" in {
      for (loggedIn <- Seq(false, true)) withClue(s"loggedIn=$loggedIn: ") {
        val result = get("/w/Places", loggedIn)
        status(result) mustBe OK
        val html = contentAsString(result)
        html must include("map-marker")
        mustNotHaveStarved(html)
      }
    }

    "draw every other screen of a page on the one connection too" in {
      val screens = Seq(
        "/w/Pool?revision=1" -> OK,
        "/w/Pool?action=history" -> OK,
        "/w/Pool?action=diff&after=2" -> OK,
        "/w/Pool?action=blame" -> OK,
        "/w/Pool?action=raw" -> OK,
        "/w/Pool?action=edit" -> OK,
        "/w/Pool?action=edit&lineStart=1&lineEnd=3" -> OK,
        "/w/Pool?action=rename" -> OK,
        "/w/Pool?action=delete" -> OK,
        "/w/NoSuchPage" -> NOT_FOUND,
        "/w/NoSuchPage?action=edit" -> OK,
      )
      for ((path, expected) <- screens; loggedIn <- Seq(false, true)) withClue(s"$path loggedIn=$loggedIn: ") {
        val result = get(path, loggedIn)
        status(result) mustBe expected
        mustNotHaveStarved(contentAsString(result))
      }
    }
  }

  // What a browser asks for after a page, and what a crawler asks for besides pages.
  "the requests around a page view" should {
    "be answered on one connection as well" in {
      val paths = Seq(
        "/api/links/Pool",
        "/api/pagePreview/Pool",
        "/api/pageRevision/Pool",
        "/api/pageNames",
        "/api/pageMap",
        // Not /search, /api/change and /api/statistics: their SQL is MySQL's and H2 will not run
        // it. All three build their context the same way, holding the connection.
        "/sitemap.xml",
        "/robots.txt",
        "/r",
      )
      // Collected rather than stopping at the first, so that one run names every path that starves.
      val starved = for {
        path <- paths
        loggedIn <- Seq(false, true)
        problem <- scala.util.Try {
          val result = get(path, loggedIn)
          val body = contentAsString(result)
          if (status(result) >= INTERNAL_SERVER_ERROR) Some(s"answered ${status(result)}")
          else if (body.contains("failed - ") || body.contains("Connection is not available")) Some("drew a failure")
          else None
        }.recover { case e => Some(e.toString.take(80)) }.get
      } yield s"$path loggedIn=$loggedIn: $problem"

      starved mustBe empty
    }

    "draw the editor's preview on one connection" in {
      val result = route(app, FakeRequest(POST, "/preview").withHeaders(HOST -> host)
        .withFormUrlEncodedBody("name" -> "Pool", "text" -> pageContent)).get
      status(result) mustBe OK
      mustRenderWhole(contentAsString(result))
    }
  }
}
