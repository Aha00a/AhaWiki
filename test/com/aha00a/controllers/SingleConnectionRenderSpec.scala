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

  override def beforeAll(): Unit = {
    super.beforeAll()
    app.injector.instanceOf[play.api.db.Database].withConnection { implicit connection =>
      TestSchema.createAll()
      Seq(
        "INSERT INTO Site (seq, name, abbr, mainDomain) VALUES (61, 'SingleConnection', 'SingleConnection', 'single-connection.test')",
        "INSERT INTO SiteDomain (site, domain) VALUES (61, 'single-connection.test')",
        "INSERT INTO Permission (site, target, targetType, actor, actorType, action) VALUES (61, '', 'All', '', 'All', 1)",
        "INSERT INTO `User` (seq, nickname) VALUES (61, 'writer')",
        "INSERT INTO Page (site, name, revision, dateTime, `user`, remoteAddress, comment, content) VALUES (61, 'Other', 1, NOW(), 61, '127.0.0.1', '', 'other-marker links to [Pool]')",
        "INSERT INTO PageMeta (site, name, revision) VALUES (61, 'Other', 1)",
      ).foreach(sql => SQL(sql).execute())
      SQL("INSERT INTO Page (site, name, revision, dateTime, `user`, remoteAddress, comment, content) VALUES (61, 'Pool', 1, NOW(), 61, '127.0.0.1', '', {content})")
        .on("content" -> pageContent).execute()
      SQL("INSERT INTO PageMeta (site, name, revision) VALUES (61, 'Pool', 1)").execute()
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

  private def mustRenderWhole(html: String): Unit = {
    html must include("pool-marker")
    html must include("other-marker")
    // What a macro or block that could not get a connection leaves in the page.
    html must not include "failed - "
    html must not include "Connection is not available"
  }

  "a page view" should {
    "render its macros, its include and its caches without asking the pool for a second connection" in {
      val result = route(app, FakeRequest(GET, "/w/Pool").withHeaders(HOST -> host)).get
      status(result) mustBe OK
      mustRenderWhole(contentAsString(result))
    }

    "do the same for a reader who is logged in" in {
      val result = route(app, FakeRequest(GET, "/w/Pool").withHeaders(HOST -> host).withSession(
        SessionLogic.sessionKeySeq -> "61",
        SessionLogic.sessionKeyNickname -> "writer",
      )).get
      status(result) mustBe OK
      mustRenderWhole(contentAsString(result))
    }
  }
}
