package com.aha00a.controllers

import actors.ActorAccessLog
import anorm._
import anorm.SqlParser.int
import com.aha00a.tests.TestApplication
import com.aha00a.tests.TestSchema
import logics.SessionLogic
import models.tables.Referral
import models.tables.Site
import org.apache.pekko.actor.ActorRef
import org.scalatest.BeforeAndAfterAll
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Application
import play.api.cache.SyncCacheApi
import play.api.db.Database
import play.api.inject.BindingKey
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.JsValue
import play.api.test.FakeRequest
import play.api.test.Helpers._

import java.sql.DriverManager
import java.time.LocalDate

/**
 * Where readers come from (evolution 73, models.tables.Referral): what counts as a referral,
 * that a repeat adds to the day's row rather than a row of its own, that the access-log actor
 * writes what the filter sends it, and that the admin report reads it back.
 *
 * Specs run without HTTP filters, so FilterAccessLog's one line that sends the message is not
 * reached here; the deploy checks it against the live site.
 */
class ReferralSpec extends PlaySpec with GuiceOneAppPerSuite with BeforeAndAfterAll {

  private val dbName = TestApplication.randomDbName("referral")

  override def fakeApplication(): Application =
    GuiceApplicationBuilder()
      .configure(TestApplication.baseConfiguration(dbName))
      .overrides(bind[SyncCacheApi].toInstance(new TestApplication.TestSyncCacheApi))
      .build()

  private def db: Database = app.injector.instanceOf[Database]

  override def beforeAll(): Unit = {
    super.beforeAll()
    Class.forName("org.h2.Driver")
    val connection = DriverManager.getConnection(TestApplication.h2Url(dbName), "sa", "")
    TestSchema.createAll()(connection)
    Seq(
      "INSERT INTO Site (seq, name, abbr) VALUES (1, 'Home', 'home')",
      "INSERT INTO SiteDomain (site, domain) VALUES (1, 'home.example.com')",
      "INSERT INTO `User` (seq, nickname) VALUES (1, 'superadmin')",
    ).foreach(sql => SQL(sql).execute()(connection))
    connection.close()
    TestApplication.resetMemoryCaches()
  }

  private val host = "home.example.com"

  private def of(referer: String, uri: String = "/w/FrontPage", method: String = "GET", status: Int = 200) =
    Referral.of(method, status, uri, host, Option(referer))

  "Referral.of" should {
    "count a page read linked from another host, by its host and path, without the query" in {
      of("https://www.google.com/search?q=wiki") mustBe Some(Referral.Visit("www.google.com/search", "/w/FrontPage"))
      of("https://GitHub.com/Aha00a/AhaWiki#readme") mustBe Some(Referral.Visit("github.com/Aha00a/AhaWiki", "/w/FrontPage"))
      of("https://other.example.com", "/") mustBe Some(Referral.Visit("other.example.com/", "/"))
    }

    "read the landing page's name, not its percent-encoding, and drop its query" in {
      of("https://blog.example.org/post", "/w/%EC%98%88%EC%8B%9C%20%ED%9A%8C%EC%9D%98%EB%A1%9D?action=edit") mustBe
        Some(Referral.Visit("blog.example.org/post", "/w/예시 회의록"))
    }

    "not count a reader moving within the site, whatever the port" in {
      of("https://home.example.com/w/Other") mustBe None
      Referral.of("GET", 200, "/w/FrontPage", "home.example.com:9000", Some("http://HOME.example.com/w/Other")) mustBe None
    }

    "not count what is not a page read" in {
      of("https://www.google.com/", "/public/img/favicon.svg") mustBe None
      of("https://www.google.com/", "/api/pageNames") mustBe None
      of("https://www.google.com/", method = "POST") mustBe None
      of("https://www.google.com/", status = 404) mustBe None
      of("https://www.google.com/", status = 303) mustBe None
    }

    "not count a Referer that is missing or not a web address" in {
      Referral.of("GET", 200, "/w/FrontPage", host, None) mustBe None
      of("") mustBe None
      of("android-app://com.google.android.gm/") mustBe None
      of("not a url at all") mustBe None
    }

    "keep at most what the columns hold" in {
      val long = "https://long.example.com/" + "a" * 400
      of(long).get.source.length mustBe Referral.MaxLength
    }
  }

  private def count(source: String): Option[Int] = db.withConnection { implicit connection =>
    SQL"SELECT `count` FROM Referral WHERE site = 1 AND source = $source".as(int("count").singleOpt)
  }

  "Referral.record" should {
    "add a repeat on the same day to that day's row" in {
      val visit = Referral.Visit("news.example.net/item", "/w/FrontPage")
      db.withConnection { implicit connection =>
        Referral.record(1, LocalDate.of(2026, 11, 15), visit)
        Referral.record(1, LocalDate.of(2026, 11, 15), visit)
        Referral.record(1, LocalDate.of(2026, 11, 14), visit)
      }
      db.withConnection { implicit connection =>
        SQL"SELECT COUNT(*) AS c FROM Referral WHERE source = 'news.example.net/item'".as(int("c").single) mustBe 2
        SQL"SELECT `count` FROM Referral WHERE source = 'news.example.net/item' AND `day` = '2026-11-15'".as(int("count").single) mustBe 2
      }
    }

    "sum the days of the window, most counted first" in {
      val rows = db.withConnection { implicit connection => Referral.since(LocalDate.of(2026, 11, 1), 10) }
      rows.map(r => (r.siteName, r.source, r.count)) mustBe Seq(("Home", "news.example.net/item", 3))
      db.withConnection { implicit connection => Referral.since(LocalDate.of(2026, 11, 15), 10) }.map(_.count) mustBe Seq(2)
    }
  }

  "the access-log actor" should {
    "write a referral it is sent, for today" in {
      val actor = app.injector.instanceOf(BindingKey(classOf[ActorRef]).qualifiedWith("access-log-actor"))
      val site = db.withConnection { implicit connection => Site.selectByAbbr("home") }.get
      actor ! ActorAccessLog.Referral(site, Referral.Visit("from.example.com/", "/w/Landing"))
      val deadline = System.currentTimeMillis() + 5000
      while (count("from.example.com/").isEmpty && System.currentTimeMillis() < deadline) Thread.sleep(50)
      count("from.example.com/") mustBe Some(1)
    }
  }

  "GET /api/Admin/Referrals" should {
    def admin(url: String) = FakeRequest(GET, url).withSession(
      SessionLogic.sessionKeySeq -> "1",
      SessionLogic.sessionKeyNickname -> "superadmin",
    )

    "answer the super admin with the window and the rows" in {
      val json = contentAsJson(route(app, admin("/api/Admin/Referrals?days=3650")).get)
      (json \ "days").as[Int] mustBe 365
      val rows = (json \ "rows").as[Seq[JsValue]]
      rows.map(r => (r \ "source").as[String]) must contain("from.example.com/")
      rows.map(r => (r \ "source").as[String]) must contain("news.example.net/item")
    }

    "refuse anyone else" in {
      status(route(app, FakeRequest(GET, "/api/Admin/Referrals")).get) mustBe FORBIDDEN
    }
  }
}
