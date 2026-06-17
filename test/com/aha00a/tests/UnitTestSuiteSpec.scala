package com.aha00a.tests

import logics.{AhaWikiCache, ApplicationConf}
import logics.wikis.RenderingMode
import models.{ContextWikiPage, PageLatestSummary, RequestWrapper, WikiActors}
import models.tables.{Site, User}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Application
import play.api.cache.SyncCacheApi
import play.api.db.Database
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.mvc.{AnyContent, Request}
import play.api.test.FakeRequest

import java.util.Locale
import scala.collection.concurrent.TrieMap
import scala.concurrent.duration.Duration
import scala.reflect.ClassTag

class UnitTestSuiteSpec extends AnyFreeSpec with GuiceOneAppPerSuite with BeforeAndAfterAll {
  private val dbName = s"legacy_unit_${java.util.UUID.randomUUID().toString.replace("-", "")}"

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
        "db.default.driver" -> "org.h2.Driver",
        "db.default.url" -> s"jdbc:h2:mem:$dbName;MODE=MySQL;NON_KEYWORDS=USER;DB_CLOSE_DELAY=-1",
        "db.default.username" -> "sa",
        "db.default.password" -> "",
        "play.evolutions.db.default.enabled" -> false,
        "play.modules.disabled" -> Seq(
          "play.api.cache.redis.RedisCacheModule",
          "services.ApplicationLifecycleHook",
        ),
        "play.http.filters" -> "play.api.http.NoHttpFilters",
        "play.http.secret.key" -> "test-secret-key-for-testing-only",
        "AhaWiki.accessLog.sampleRate" -> 0,
      )
      .overrides(bind[SyncCacheApi].toInstance(new TestSyncCacheApi))
      .build()
  }

  private implicit lazy val request: Request[AnyContent] =
    FakeRequest("GET", "/").withHeaders("Host" -> "localhost")

  private implicit lazy val database: Database = app.injector.instanceOf[Database]
  private implicit lazy val wikiActors: WikiActors = WikiActors(null, null)
  private implicit lazy val applicationConf: ApplicationConf = app.injector.instanceOf[ApplicationConf]
  private implicit lazy val ahaWikiCache: AhaWikiCache = app.injector.instanceOf[AhaWikiCache]
  private implicit lazy val site: Site = Site(1, "UnitTest", "UnitTest", "localhost")

  private implicit lazy val requestWrapper: RequestWrapper = new RequestWrapper {
    override def getUser: Option[User.SessionUser] = None
    override def getUserProfileImageUrl: Option[String] = None
    override def locale: Locale = Locale.US
    override def getQueryString(key: String): Option[String] = request.getQueryString(key)
    override val remoteAddress: String = "127.0.0.1"
    override def flashGet(key: String): Option[String] = None
    override def host: String = request.host
  }

  private implicit lazy val contextWikiPage: ContextWikiPage =
    new ContextWikiPage(Seq("UnitTest"), RenderingMode.Normal) {
      override lazy val setPageName: Set[String] = Set("FrontPage")
      override lazy val seqPageByPermission: Seq[PageLatestSummary] = Seq.empty
      override lazy val seqPageNameByPermission: Seq[String] = Seq("FrontPage")
      override lazy val setPageNameByPermission: Set[String] = Set("FrontPage")
      override lazy val defaultHue: Option[Int] = None
    }

  private val testUtil = new TestUtil(_ => ())

  private def setupSchema(): Unit = {
    database.withConnection { connection =>
      Seq(
        """
          CREATE TABLE IF NOT EXISTS User (
            seq BIGINT AUTO_INCREMENT PRIMARY KEY,
            nickname VARCHAR(32) NOT NULL
          )
        """,
        """
          CREATE TABLE IF NOT EXISTS Page (
            site BIGINT NOT NULL,
            name VARCHAR(255) NOT NULL,
            revision BIGINT NOT NULL,
            dateTime DATETIME DEFAULT NOW() NOT NULL,
            user BIGINT NULL,
            remoteAddress VARCHAR(255) NOT NULL DEFAULT '',
            comment VARCHAR(255) NOT NULL DEFAULT '',
            isMinorEdit BOOLEAN NOT NULL DEFAULT FALSE,
            content CLOB NOT NULL,
            PRIMARY KEY (site, name, revision)
          )
        """,
      ).foreach { sql =>
        val statement = connection.createStatement()
        try statement.execute(sql)
        finally statement.close()
      }
    }
  }

  override def beforeAll(): Unit = {
    super.beforeAll()
    setupSchema()
  }

  "legacy app unit tests" - {
    UnitTestSuite.cases(testUtil).foreach { testCase =>
      testCase.name in {
        testCase.run()
      }
    }
  }
}
