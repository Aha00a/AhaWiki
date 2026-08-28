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
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers._

/** The three surfaces that must apply read permissions, exercised through the router: the
  * page list (`/api/pageNames`), search (`/search`), and `[[Include(...)]]`. Each one calls
  * `WikiPermission.isReadable` itself, so each one can forget to — a page that never renders
  * on its own URL still leaks through a list, a search summary, or an include if any of the
  * three skips the check. AccessControl's row semantics are covered by `PermissionLogicUnit`;
  * this spec covers the surfaces that consume them.
  *
  * The site lives on its own seq and host (`acl.test`): `AhaWikiCacheMemoryPermission` is a
  * JVM-wide singleton keyed by site seq, so sharing seq 1 with the other specs would hand
  * this spec whichever permission rows got cached first.
  */
class AccessControlFilteringSpec extends PlaySpec with GuiceOneAppPerSuite with BeforeAndAfterAll {

  private val dbName = TestApplication.randomDbName("acl_filtering")
  private val host = "acl.test"

  override def fakeApplication(): Application = {
    GuiceApplicationBuilder()
      .configure(TestApplication.baseConfiguration(dbName))
      .overrides(bind[SyncCacheApi].toInstance(new TestApplication.TestSyncCacheApi))
      .build()
  }

  override def beforeAll(): Unit = {
    super.beforeAll()
    // Through the app's own pool, the way ApiV1Spec does it — the include path below reads
    // pages with `Page.selectLastRevision`, whose camelCase column labels only survive when
    // the same connection settings created the schema.
    app.injector.instanceOf[play.api.db.Database].withConnection { implicit connection =>
      TestSchema.createAll()
    Seq(
      "INSERT INTO Site (seq, name, abbr, mainDomain) VALUES (7, 'AclWiki', 'AclWiki', 'acl.test')",
      "INSERT INTO SiteDomain (site, domain) VALUES (7, 'acl.test')",
      "INSERT INTO `User` (seq, nickname) VALUES (21, 'reader')",
      "INSERT INTO UserEmail (`user`, email, isPrimary) VALUES (21, 'reader@example.com', TRUE)",
      // remoteAddress must be set: the schema allows NULL but `rowParserPage` reads it with
      // a non-optional str(), and anorm reports a NULL under a strict parser as
      // "'remoteAddress' not found" — a message that sends you hunting for a column that is
      // right there in its own "available columns" list. Cost this spec an evening's detour.
      // Production is safe from the same trap — measured 2026-08-29: 0 of 22,719 Page rows
      // carry a NULL there, because the application always writes the requester's address.
      "INSERT INTO Page (site, name, revision, dateTime, `user`, remoteAddress, comment, content) " +
        "VALUES (7, 'PublicPage', 1, NOW(), 21, '127.0.0.1', '', 'acl-public-marker searchtoken')",
      "INSERT INTO Page (site, name, revision, dateTime, `user`, remoteAddress, comment, content) " +
        "VALUES (7, 'PrivatePage', 1, NOW(), 21, '127.0.0.1', '', 'acl-private-marker searchtoken')",
      // The page list serves PageMeta, not Page — a page without its meta row is not listed.
      "INSERT INTO PageMeta (site, name, revision) VALUES (7, 'PublicPage', 1)",
      "INSERT INTO PageMeta (site, name, revision) VALUES (7, 'PrivatePage', 1)",
      // Everyone reads by default; PrivatePage is blocked for everyone except one exact
      // actor — the Exact+Exact row outranks the Exact+All block by specificity.
      "INSERT INTO Permission (site, target, targetType, actor, actorType, action) VALUES (7, '', 'All', '', 'All', 1)",
      "INSERT INTO Permission (site, target, targetType, actor, actorType, action) VALUES (7, 'PrivatePage', 'Exact', '', 'All', 0)",
      "INSERT INTO Permission (site, target, targetType, actor, actorType, action) VALUES (7, 'PrivatePage', 'Exact', 'reader@example.com', 'Exact', 1)",
    ).foreach(sql => SQL(sql).execute())
    }
    TestApplication.resetMemoryCaches()
  }

  // This suite's requests refill the singletons from its own database; empty them again so
  // a later suite that forgot to defend itself does not read acl.test's sites as the world.
  override def afterAll(): Unit = {
    TestApplication.resetMemoryCaches()
    super.afterAll()
  }

  private def anonymous(method: String, url: String) =
    FakeRequest(method, url).withHeaders(HOST -> host)

  private def asReader(method: String, url: String) =
    anonymous(method, url).withSession(
      SessionLogic.sessionKeySeq -> "21",
      SessionLogic.sessionKeyNickname -> "reader",
    )

  "the page list" should {
    "leave an unreadable page out for anonymous" in {
      val result = route(app, anonymous(GET, "/api/pageNames")).get
      status(result) mustBe OK
      val names = contentAsJson(result).as[Seq[String]]
      names must contain("PublicPage")
      names must not contain "PrivatePage"
    }

    "include it for the one actor its Exact row admits" in {
      val result = route(app, asReader(GET, "/api/pageNames")).get
      status(result) mustBe OK
      contentAsJson(result).as[Seq[String]] must contain allOf ("PublicPage", "PrivatePage")
    }
  }

  // Search is the third surface and is NOT covered here: `Page.pageSearch` forces
  // `COLLATE utf8mb4_general_ci` onto its LIKEs — load-bearing, because the tables are
  // `utf8mb4_bin` and search must not be case-sensitive — and H2 rejects that clause even
  // in MySQL mode. Its permission filter is the same `WikiPermission.isReadable` call the
  // two surfaces below exercise, one line above the summary building (Search.scala).
  // Removing the COLLATE to make this testable would silently make production search
  // case-sensitive; a spec is not worth that trade.

  "[[Include(...)]]" should {
    "render Permission Denied instead of the unreadable page's content" in {
      val result = route(app, anonymous(POST, "/api/renderAhaMark/SomePage")
        .withJsonBody(Json.obj("comment" -> "[[Include(PrivatePage)]]"))).get
      status(result) mustBe OK
      val html = (contentAsJson(result) \ "html").as[String]
      html must include("Permission Denied")
      html must not include "acl-private-marker"
    }

    "render a readable page's content, so the denial above is the permission and not a bug" in {
      val result = route(app, anonymous(POST, "/api/renderAhaMark/SomePage")
        .withJsonBody(Json.obj("comment" -> "[[Include(PublicPage)]]"))).get
      status(result) mustBe OK
      (contentAsJson(result) \ "html").as[String] must include("acl-public-marker")
    }

    "render the private page for the admitted actor" in {
      val result = route(app, asReader(POST, "/api/renderAhaMark/SomePage")
        .withJsonBody(Json.obj("comment" -> "[[Include(PrivatePage)]]"))).get
      status(result) mustBe OK
      (contentAsJson(result) \ "html").as[String] must include("acl-private-marker")
    }
  }
}
