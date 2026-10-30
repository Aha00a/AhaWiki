package com.aha00a.controllers

import anorm.SQL
import com.aha00a.tests.TestApplication
import com.aha00a.tests.TestSchema
import controllers.ApiAdminReport
import logics.SessionLogic
import org.scalatest.BeforeAndAfterAll
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Application
import play.api.cache.SyncCacheApi
import play.api.db.Database
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.FakeRequest
import play.api.test.Helpers._

import java.sql.DriverManager
import java.time.LocalDateTime

/**
 * The site activity report, on a fixed clock. `now` is 2026-10-31 15:00; the twelve months
 * run 2025-11 to 2026-10 and the 30-day window opens 2026-10-01 15:00.
 *
 * Two sites. Site 1 was created in 2026-03 and has a page from 2026-03 (outside the window),
 * three saves of another page in October (inside), and one older than the series. Site 2 was
 * created in 2026-10 and has never been written to — the row the report exists to show, since
 * a site that is created and then left is the failure the acquisition work is trying to see.
 */
class ApiAdminSiteActivitySpec extends PlaySpec with GuiceOneAppPerSuite with BeforeAndAfterAll {

  private val dbName = TestApplication.randomDbName("api_siteactivity")

  private val now = LocalDateTime.of(2026, 10, 31, 15, 0)

  override def fakeApplication(): Application = {
    GuiceApplicationBuilder()
      .configure(TestApplication.baseConfiguration(dbName))
      .overrides(bind[SyncCacheApi].toInstance(new TestApplication.TestSyncCacheApi))
      .build()
  }

  private def db: Database = app.injector.instanceOf[Database]

  private def page(site: Int, name: String, revision: Int, dateTime: String, user: Int): String =
    "INSERT INTO Page (site, name, revision, dateTime, `user`, remoteAddress, comment, content) " +
      s"VALUES ($site, '$name', $revision, '$dateTime', $user, '127.0.0.1', '', '')"

  private def setupSchema(): Unit = {
    Class.forName("org.h2.Driver")
    val connection = DriverManager.getConnection(TestApplication.h2Url(dbName), "sa", "")
    TestSchema.createAll()(connection)
    Seq(
      "INSERT INTO Site (seq, name, abbr, created) VALUES (1, 'Busy', 'busy', '2026-03-05 10:00:00')",
      "INSERT INTO Site (seq, name, abbr, created) VALUES (2, 'Idle', 'idle', '2026-10-20 10:00:00')",
      "INSERT INTO SiteDomain (site, domain) VALUES (1, 'busy.example.com')",
      "INSERT INTO `User` (seq, nickname) VALUES (1, 'superadmin')",
      "INSERT INTO `User` (seq, nickname) VALUES (10, 'alice')",
      "INSERT INTO `User` (seq, nickname) VALUES (11, 'bob')",
      // Before the series: must not appear in any month, but still the site's history.
      page(1, "Ancient", 1, "2024-01-01 09:00:00", 10),
      // In the series, outside the 30-day window.
      page(1, "Spring", 1, "2026-03-06 09:00:00", 10),
      // Inside the window: one page created, then edited twice, by two people.
      page(1, "October", 1, "2026-10-02 09:00:00", 10),
      page(1, "October", 2, "2026-10-10 09:00:00", 11),
      page(1, "October", 3, "2026-10-30 09:00:00", 10),
    ).foreach(sql => SQL(sql).execute()(connection))
    connection.close()
  }

  override def beforeAll(): Unit = {
    super.beforeAll()
    setupSchema()
    TestApplication.resetMemoryCaches()
  }

  private def adminRequest(method: String, url: String) =
    FakeRequest(method, url).withSession(
      SessionLogic.sessionKeySeq -> "1",
      SessionLogic.sessionKeyNickname -> "superadmin",
    )

  "ApiAdminReport.siteActivity" should {
    lazy val report = db.withConnection { implicit connection => ApiAdminReport.siteActivity(now) }

    "return every month of the series, oldest first, with zeros where nothing happened" in {
      report.months.map(_.ym) mustBe Seq(
        "2025-11", "2025-12", "2026-01", "2026-02", "2026-03", "2026-04",
        "2026-05", "2026-06", "2026-07", "2026-08", "2026-09", "2026-10",
      )
      val quiet = report.months.find(_.ym == "2026-06").get
      quiet.sitesCreated mustBe 0
      quiet.sitesActive mustBe 0
      quiet.pagesCreated mustBe 0
      quiet.pagesEdited mustBe 0
    }

    "count a site as created in the month of its Site.created" in {
      report.months.find(_.ym == "2026-03").get.sitesCreated mustBe 1
      report.months.find(_.ym == "2026-10").get.sitesCreated mustBe 1
    }

    "count a site as active in a month when any revision was saved there" in {
      val march = report.months.find(_.ym == "2026-03").get
      march.sitesActive mustBe 1
      march.pagesCreated mustBe 1
      march.pagesEdited mustBe 1

      val october = report.months.find(_.ym == "2026-10").get
      october.sitesActive mustBe 1
      october.pagesCreated mustBe 1
      october.pagesEdited mustBe 3
    }

    "list every site, the busiest first, with its 30-day counts and first and last edit" in {
      report.sites.map(_.siteSeq) mustBe Seq(1L, 2L)

      val busy = report.sites.head
      busy.siteName mustBe "Busy"
      busy.siteDomain mustBe Some("busy.example.com")
      busy.created mustBe "2026-03-05 10:00:00"
      busy.pagesCreated30d mustBe 1
      busy.pagesEdited30d mustBe 3
      busy.editors30d mustBe 2
      busy.firstEditedAt mustBe Some("2024-01-01 09:00:00")
      busy.lastEditedAt mustBe Some("2026-10-30 09:00:00")

      val idle = report.sites(1)
      idle.siteDomain mustBe None
      idle.pagesCreated30d mustBe 0
      idle.pagesEdited30d mustBe 0
      idle.editors30d mustBe 0
      idle.firstEditedAt mustBe None
      idle.lastEditedAt mustBe None
    }
  }

  "GET /api/Admin/SiteActivity" should {
    "return 403 for anonymous user" in {
      status(route(app, FakeRequest(GET, "/api/Admin/SiteActivity")).get) mustBe FORBIDDEN
    }

    "return the report to the admin" in {
      val result = route(app, adminRequest(GET, "/api/Admin/SiteActivity")).get
      status(result) mustBe OK
      val json = contentAsJson(result)
      (json \ "months").as[Seq[play.api.libs.json.JsValue]].size mustBe ApiAdminReport.monthsReported
      (json \ "sites").as[Seq[play.api.libs.json.JsValue]].map(s => (s \ "siteName").as[String]) must contain("Idle")
    }
  }
}
