package com.aha00a.controllers

import anorm.SQL
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

import scala.collection.concurrent.TrieMap
import scala.concurrent.duration.Duration
import scala.reflect.ClassTag

class BotApiFilterSpec extends PlaySpec with GuiceOneAppPerSuite with BeforeAndAfterAll {

  private val dbName = s"bot_api_filter_${java.util.UUID.randomUUID().toString.replace("-", "")}"
  private val actorSystem = ActorSystem(s"$dbName-actors")

  private class TestSyncCacheApi extends SyncCacheApi {
    private val values = TrieMap.empty[String, Any]

    override def set(key: String, value: Any, expiration: Duration): Unit =
      values.put(key, value)

    override def remove(key: String): Unit =
      values.remove(key)

    override def getOrElseUpdate[A](key: String, expiration: Duration)(orElse: => A)(implicit evidence$1: ClassTag[A]): A =
      values.getOrElseUpdate(key, orElse).asInstanceOf[A]

    override def get[T](key: String)(implicit evidence$2: ClassTag[T]): Option[T] =
      values.get(key).map(_.asInstanceOf[T])
  }

  override def fakeApplication(): Application = {
    GuiceApplicationBuilder()
      .configure(
        "db.default.driver"   -> "org.h2.Driver",
        "db.default.url"      -> s"jdbc:h2:mem:$dbName;MODE=MySQL;NON_KEYWORDS=USER;DB_CLOSE_DELAY=-1",
        "db.default.username" -> "sa",
        "db.default.password" -> "",
        "play.evolutions.db.default.enabled" -> false,
        "play.modules.disabled" -> Seq(
          "play.api.cache.redis.RedisCacheModule",
          "services.ApplicationLifecycleHook",
        ),
        "play.http.filters" -> "Filters",
        "play.http.secret.key" -> "test-secret-key-for-testing-only",
        "AhaWiki.accessLog.sampleRate" -> 0,
      )
      .overrides(
        bind[SyncCacheApi].toInstance(new TestSyncCacheApi),
        bind[WikiActors].toInstance(WikiActors(actorSystem.deadLetters, actorSystem.deadLetters)),
        bind[ActorRef].qualifiedWith("access-log-actor").toInstance(actorSystem.deadLetters),
      )
      .build()
  }

  private def db: Database = app.injector.instanceOf[Database]
  private implicit val site: Site = Site(1, "TestWiki", "TestWiki", "localhost")

  private def setupSchema(): Unit = {
    db.withConnection { implicit connection =>
      Seq(
        """
          CREATE TABLE Site (
            seq INT AUTO_INCREMENT PRIMARY KEY,
            created DATETIME DEFAULT NOW() NOT NULL,
            updated DATETIME DEFAULT NOW() NOT NULL,
            name VARCHAR(200) NOT NULL,
            abbr VARCHAR(200) NOT NULL DEFAULT '',
            mainDomain VARCHAR(255) NOT NULL DEFAULT '',
            publicListedOrder DECIMAL(10, 2) NULL
          )
        """,
        """
          CREATE TABLE SiteDomain (
            created DATETIME DEFAULT NOW() NOT NULL,
            site INT NOT NULL,
            domain VARCHAR(255) NOT NULL,
            PRIMARY KEY (site, domain)
          )
        """,
        """
          CREATE TABLE User (
            seq INT AUTO_INCREMENT PRIMARY KEY,
            created DATETIME DEFAULT NOW() NOT NULL,
            updated DATETIME DEFAULT NOW() NOT NULL,
            nickname VARCHAR(32) NOT NULL,
            profileImageUrl VARCHAR(255) NULL
          )
        """,
        """
          CREATE TABLE UserEmail (
            `user` INT NOT NULL,
            email VARCHAR(255) NOT NULL,
            isPrimary BOOLEAN NOT NULL DEFAULT FALSE,
            created DATETIME DEFAULT NOW() NOT NULL,
            PRIMARY KEY (`user`, email),
            UNIQUE (email)
          )
        """,
        """
          CREATE TABLE Permission (
            site INT NOT NULL,
            target VARCHAR(255) NOT NULL,
            targetType VARCHAR(255) NOT NULL,
            actor VARCHAR(255) NOT NULL,
            actorType VARCHAR(255) NOT NULL,
            action INT NOT NULL,
            dateUpdated DATETIME DEFAULT NOW() NOT NULL,
            PRIMARY KEY (site, target, targetType, actor, actorType)
          )
        """,
        """
          CREATE TABLE Page (
            site INT NOT NULL,
            name VARCHAR(255) NOT NULL,
            revision BIGINT NOT NULL,
            dateTime DATETIME DEFAULT NOW() NOT NULL,
            `user` INT NULL,
            remoteAddress VARCHAR(255) NOT NULL DEFAULT '',
            comment VARCHAR(255) NOT NULL DEFAULT '',
            isMinorEdit BOOLEAN NOT NULL DEFAULT FALSE,
            viaApi BOOLEAN NOT NULL DEFAULT FALSE,
            content CLOB NOT NULL,
            PRIMARY KEY (site, name, revision)
          )
        """,
        """
          CREATE TABLE UserApiKey (
            seq BIGINT NOT NULL AUTO_INCREMENT,
            `user` INT NOT NULL,
            keyHash VARCHAR(64) NOT NULL,
            keyPrefix VARCHAR(32) NOT NULL,
            label VARCHAR(255) NOT NULL,
            dateInserted DATETIME NOT NULL DEFAULT NOW(),
            dateLastUsed DATETIME NULL,
            dateRevoked DATETIME NULL,
            PRIMARY KEY (seq),
            UNIQUE (keyHash)
          )
        """,
        """
          CREATE TABLE IpDeny (
            seq BIGINT NOT NULL AUTO_INCREMENT,
            accessLog BIGINT NULL,
            dateInserted DATETIME DEFAULT NOW() NOT NULL,
            ip VARCHAR(46) NOT NULL,
            reason VARCHAR(255) NOT NULL DEFAULT '',
            PRIMARY KEY (seq)
          )
        """,
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

  "POST /api/bot/page/:name through Filters" should {
    "bypass CSRF and save with Bearer authentication only" in {
      val created = createApiKey()
      db.withConnection { implicit connection =>
        Page.insert(Page("BotCsrf", 1, java.time.LocalDateTime.now(), None, Some(1), "127.0.0.1", "", isMinorEdit = false, "= BotCsrf\nold"))
      }

      val result = route(app, FakeRequest(POST, "/api/bot/page/BotCsrf")
        .withHeaders(HOST -> "localhost", AUTHORIZATION -> s"Bearer ${created.rawKey}")
        .withJsonBody(Json.obj(
          "revision" -> 1,
          "text" -> "= BotCsrf\nnew",
        ))).get

      status(result) mustBe OK

      db.withConnection { implicit connection =>
        val page = Page.selectLastRevision("BotCsrf").get
        page.revision mustBe 2
        page.viaApi mustBe true
      }
    }
  }
}
