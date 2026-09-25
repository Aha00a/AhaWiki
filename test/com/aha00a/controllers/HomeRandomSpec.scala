package com.aha00a.controllers

import anorm.SQL
import com.aha00a.tests.TestApplication
import com.aha00a.tests.TestSchema
import models.WikiActors
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

/**
 * `/random` sends the visitor to one of the pages they may read. Picking one out of none threw
 * `IllegalArgumentException: bound must be positive`, which the reader got as a 500 — a visitor
 * to a wiki whose pages are all private to them could hit it by pressing Random, and the
 * production log had it once or twice a day.
 *
 * Its own site seq and host, for the reason AccessControlFilteringSpec gives: the permission
 * cache is a JVM-wide singleton keyed by site seq.
 */
class HomeRandomSpec extends PlaySpec with GuiceOneAppPerSuite with BeforeAndAfterAll {

  private val dbName = TestApplication.randomDbName("random")
  private val actorSystem = ActorSystem(s"$dbName-actors")
  private val host = "random.test"
  private implicit val site: Site = Site(11, "RandomWiki", "RandomWiki", host)

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
        s"INSERT INTO Site (seq, name, abbr, mainDomain) VALUES (11, 'RandomWiki', 'RandomWiki', '$host')",
        s"INSERT INTO SiteDomain (site, domain) VALUES (11, '$host')",
        "INSERT INTO `User` (seq, nickname) VALUES (51, 'author')",
        // A page exists, and no Permission row lets anyone read it. The page list serves PageMeta,
        // so the meta row has to be there for the page to be a candidate at all.
        "INSERT INTO Page (site, name, revision, dateTime, `user`, remoteAddress, comment, content) " +
          "VALUES (11, 'Secret', 1, NOW(), 51, '127.0.0.1', '', '= Secret')",
        "INSERT INTO PageMeta (site, name, revision) VALUES (11, 'Secret', 1)",
      ).foreach(sql => SQL(sql).execute())
    }
    TestApplication.resetMemoryCaches()
  }

  override def afterAll(): Unit = {
    TestApplication.resetMemoryCaches()
    actorSystem.terminate()
    super.afterAll()
  }

  "GET /r (Random)" should {
    "send a visitor who may read nothing to the front page, not answer 500" in {
      val result = route(app, FakeRequest(GET, "/r").withHeaders(HOST -> host)).get
      status(result) mustBe SEE_OTHER
      redirectLocation(result) mustBe Some("/w/FrontPage")
    }
  }
}
