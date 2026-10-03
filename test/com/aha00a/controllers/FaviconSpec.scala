package com.aha00a.controllers

import anorm.SQL
import com.aha00a.tests.TestApplication
import com.aha00a.tests.TestSchema
import org.scalatest.BeforeAndAfterAll
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Application
import play.api.cache.SyncCacheApi
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.FakeRequest
import play.api.test.Helpers._

/** /favicon.ico, which browsers and crawlers ask every host for and MacroAhaWikiSiteList points
  * each listed site's icon at. It used to answer 404 everywhere; it now sends the request on to
  * the site's own favicon (controllers.Home.favicon, logics.AhaWikiConfig).
  *
  * The sites have seqs and hosts of their own, because the memory caches are shared by every spec
  * in the JVM and keyed by site. */
class FaviconSpec extends PlaySpec with GuiceOneAppPerSuite with BeforeAndAfterAll {

  override def fakeApplication(): Application =
    GuiceApplicationBuilder()
      .configure(TestApplication.baseConfiguration(TestApplication.randomDbName("favicon")))
      .overrides(bind[SyncCacheApi].toInstance(new TestApplication.TestSyncCacheApi))
      .build()

  override def beforeAll(): Unit = {
    super.beforeAll()
    app.injector.instanceOf[play.api.db.Database].withConnection { implicit connection =>
      TestSchema.createAll()
      Seq(
        "INSERT INTO Site (seq, name, abbr, mainDomain) VALUES (71, 'Plain', 'Plain', 'favicon-plain.test')",
        "INSERT INTO SiteDomain (site, domain) VALUES (71, 'favicon-plain.test')",
        "INSERT INTO Site (seq, name, abbr, mainDomain) VALUES (72, 'Path', 'Path', 'favicon-path.test')",
        "INSERT INTO SiteDomain (site, domain) VALUES (72, 'favicon-path.test')",
        "INSERT INTO Config (site, k, v) VALUES (72, 'site.favicon.objectKey', '/public/img/custom.png')",
        "INSERT INTO Site (seq, name, abbr, mainDomain) VALUES (73, 'Url', 'Url', 'favicon-url.test')",
        "INSERT INTO SiteDomain (site, domain) VALUES (73, 'favicon-url.test')",
        "INSERT INTO Config (site, k, v) VALUES (73, 'site.favicon.objectKey', 'https://cdn.example/icon.png')",
      ).foreach(sql => SQL(sql).execute())
    }
    TestApplication.resetMemoryCaches()
  }

  override def afterAll(): Unit = {
    TestApplication.resetMemoryCaches()
    super.afterAll()
  }

  private def favicon(host: String) = route(app, FakeRequest(GET, "/favicon.ico").withHeaders(HOST -> host)).get

  "/favicon.ico" should {
    "send a site without a favicon of its own to the default one" in {
      val result = favicon("favicon-plain.test")
      status(result) mustBe FOUND
      redirectLocation(result) mustBe Some(logics.AhaWikiConfig.DefaultFaviconPath)
      header(CACHE_CONTROL, result) mustBe Some("public, max-age=3600")
    }

    "send a site to the path it has configured" in {
      redirectLocation(favicon("favicon-path.test")) mustBe Some("/public/img/custom.png")
    }

    "send a site to the URL it has configured" in {
      redirectLocation(favicon("favicon-url.test")) mustBe Some("https://cdn.example/icon.png")
    }

    "send a host that is no site to the default one rather than failing" in {
      val result = favicon("no-such-site.test")
      status(result) mustBe FOUND
      redirectLocation(result) mustBe Some(logics.AhaWikiConfig.DefaultFaviconPath)
    }
  }
}
