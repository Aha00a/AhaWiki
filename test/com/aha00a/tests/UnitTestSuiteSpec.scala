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

class UnitTestSuiteSpec extends AnyFreeSpec with GuiceOneAppPerSuite with BeforeAndAfterAll {
  private val dbName = TestApplication.randomDbName("unit_suite")


  override def fakeApplication(): Application = {
    GuiceApplicationBuilder()
      .configure(TestApplication.baseConfiguration(dbName))
      .overrides(bind[SyncCacheApi].toInstance(new TestApplication.TestSyncCacheApi))
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
      TestSchema.createAll()(connection)
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
