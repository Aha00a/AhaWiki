package com.aha00a.controllers

import anorm.SQL
import com.aha00a.tests.TestApplication
import com.aha00a.tests.TestSchema
import logics.SessionLogic
import models.WikiActors
import models.tables.Page
import models.tables.Site
import org.apache.pekko.actor.ActorSystem
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

import java.time.LocalDate

/** `/diary` appends a line to today's page, creating it when it is missing. Creating a page
  * needs `Create` everywhere else; until 2026-09-12 the diary passed the missing page's empty
  * text as if it were an existing page, so `Edit` alone was enough.
  *
  * Its own site seq and host, for the reason AccessControlFilteringSpec gives: the permission
  * cache is a JVM-wide singleton keyed by site seq. */
class DiarySpec extends PlaySpec with GuiceOneAppPerSuite with BeforeAndAfterAll {

  private val dbName = TestApplication.randomDbName("diary")
  private val actorSystem = ActorSystem(s"$dbName-actors")
  private val host = "diary.test"
  private implicit val site: Site = Site(8, "DiaryWiki", "DiaryWiki", host)

  override def fakeApplication(): Application =
    GuiceApplicationBuilder()
      .configure(TestApplication.baseConfiguration(dbName))
      .overrides(
        bind[SyncCacheApi].toInstance(new TestApplication.TestSyncCacheApi),
        bind[WikiActors].toInstance(WikiActors(actorSystem.deadLetters, actorSystem.deadLetters)),
      )
      .build()

  private def db: Database = app.injector.instanceOf[Database]

  override def beforeAll(): Unit = {
    super.beforeAll()
    db.withConnection { implicit connection =>
      TestSchema.createAll()
      Seq(
        s"INSERT INTO Site (seq, name, abbr, mainDomain) VALUES (8, 'DiaryWiki', 'DiaryWiki', '$host')",
        s"INSERT INTO SiteDomain (site, domain) VALUES (8, '$host')",
        "INSERT INTO `User` (seq, nickname) VALUES (31, 'editor')",
        "INSERT INTO UserEmail (`user`, email, isPrimary) VALUES (31, 'editor@example.com', TRUE)",
        "INSERT INTO `User` (seq, nickname) VALUES (32, 'creator')",
        "INSERT INTO UserEmail (`user`, email, isPrimary) VALUES (32, 'creator@example.com', TRUE)",
        // Edit(2) for one, Create(4) for the other, on every page of the site.
        "INSERT INTO Permission (site, target, targetType, actor, actorType, action) VALUES (8, '', 'All', 'editor@example.com', 'Exact', 2)",
        "INSERT INTO Permission (site, target, targetType, actor, actorType, action) VALUES (8, '', 'All', 'creator@example.com', 'Exact', 4)",
      ).foreach(sql => SQL(sql).execute())
    }
    TestApplication.resetMemoryCaches()
  }

  override def afterAll(): Unit = {
    TestApplication.resetMemoryCaches()
    actorSystem.terminate()
    super.afterAll()
  }

  private def write(seq: Long, nickname: String, line: String) =
    route(app, FakeRequest(POST, "/diary")
      .withHeaders(HOST -> host)
      .withSession(SessionLogic.sessionKeySeq -> seq.toString, SessionLogic.sessionKeyNickname -> nickname)
      .withFormUrlEncodedBody("q" -> line)).get

  private def today: Option[Page] = db.withConnection { implicit connection =>
    Page.selectLastRevision(LocalDate.now().toString)
  }

  "POST /diary" should {
    "not let Edit alone create today's page" in {
      status(write(31, "editor", "first line")) mustBe SEE_OTHER
      today mustBe None
    }

    "let Create create it" in {
      status(write(32, "creator", "first line")) mustBe SEE_OTHER
      today.map(_.revision) mustBe Some(1)
      today.map(_.content).getOrElse("") must include("first line")
    }

    "let Edit add to it once it exists" in {
      status(write(31, "editor", "second line")) mustBe SEE_OTHER
      today.map(_.revision) mustBe Some(2)
      today.map(_.content).getOrElse("") must include("second line")
    }
  }
}
