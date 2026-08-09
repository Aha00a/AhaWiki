package com.aha00a.controllers

import anorm.SQL
import com.aha00a.tests.TestApplication
import com.aha00a.tests.TestSchema
import logics.AhaWikiCacheMemoryDomainSite
import logics.AhaWikiCacheMemoryPermission
import models.WikiActors
import models.tables.Page
import models.tables.Site
import models.tables.UserApiKey
import org.apache.pekko.actor.{ActorRef, ActorSystem}
import org.scalatest.BeforeAndAfterAll
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Application
import play.api.cache.SyncCacheApi
import play.api.db.Database
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers._


class ApiV1FilterSpec extends PlaySpec with GuiceOneAppPerSuite with BeforeAndAfterAll {

  private val dbName = TestApplication.randomDbName("api_v1_filter")
  private val actorSystem = ActorSystem(s"$dbName-actors")


  override def fakeApplication(): Application = {
    GuiceApplicationBuilder()
      .configure(TestApplication.baseConfiguration(dbName) ++ Map("play.http.filters" -> "Filters"))
      .overrides(
        bind[SyncCacheApi].toInstance(new TestApplication.TestSyncCacheApi),
        bind[WikiActors].toInstance(WikiActors(actorSystem.deadLetters, actorSystem.deadLetters)),
        bind[ActorRef].qualifiedWith("access-log-actor").toInstance(actorSystem.deadLetters),
      )
      .build()
  }

  private def db: Database = app.injector.instanceOf[Database]
  private implicit val site: Site = Site(1, "TestWiki", "TestWiki", "localhost")

  private def setupSchema(): Unit = {
    db.withConnection { implicit connection =>
      TestSchema.create("Site", "SiteDomain", "User", "UserEmail", "Permission", "Page", "UserApiKey", "IpDeny")
      Seq(
        "INSERT INTO Site (seq, name, abbr, mainDomain) VALUES (1, 'TestWiki', 'TestWiki', 'localhost')",
        "INSERT INTO SiteDomain (site, domain) VALUES (1, 'localhost')",
        "INSERT INTO User (seq, nickname) VALUES (1, 'alice')",
        "INSERT INTO UserEmail (`user`, email, isPrimary) VALUES (1, 'alice@example.com', TRUE)",
        "INSERT INTO Permission (site, target, targetType, actor, actorType, action) VALUES (1, '', 'All', '', 'Login', 255)",
      ).foreach(sql => SQL(sql).execute())
    }
  }

  private def createApiKey(): UserApiKey.Created =
    db.withConnection { implicit connection =>
      UserApiKey.insert(1, "filter key")
    }

  override def beforeAll(): Unit = {
    super.beforeAll()
    setupSchema()
    AhaWikiCacheMemoryDomainSite.invalidate()(db)
    AhaWikiCacheMemoryPermission.clear()
  }

  override def afterAll(): Unit = {
    actorSystem.terminate()
    super.afterAll()
  }

  "POST /api/v1/page/:name through Filters" should {
    "bypass CSRF and save with Bearer authentication only" in {
      val created = createApiKey()
      db.withConnection { implicit connection =>
        Page.insert(Page("ApiCsrf", 1, java.time.LocalDateTime.now(), None, Some(1), "127.0.0.1", "", isMinorEdit = false, "= ApiCsrf\nold"))
      }

      val result = route(app, FakeRequest(POST, "/api/v1/page/ApiCsrf")
        .withHeaders(HOST -> "localhost", AUTHORIZATION -> s"Bearer ${created.rawKey}")
        .withJsonBody(Json.obj(
          "revision" -> 1,
          "text" -> "= ApiCsrf\nnew",
        ))).get

      status(result) mustBe OK

      db.withConnection { implicit connection =>
        val page = Page.selectLastRevision("ApiCsrf").get
        page.revision mustBe 2
        page.viaApi mustBe true
      }
    }
  }
}
