package com.aha00a.controllers

import anorm.SQL
import com.aha00a.tests.TestApplication
import com.aha00a.tests.TestSchema
import logics.PublicAsset
import org.scalatest.BeforeAndAfterAll
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Application
import play.api.cache.SyncCacheApi
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.FakeRequest
import play.api.test.Helpers._

/** The addresses a page gives the wiki's own CSS and JS.
  *
  * /public/ goes out with max-age=3600, and until 2026-09-27 each file had one fixed address, so a
  * browser that had fetched it in the hour before a deploy drew the new HTML with the old file. Each
  * address now carries a digest of the file (logics.PublicAsset). What this pins is that the digest
  * a page names is the digest of what the server sends at that address -- the address changes
  * exactly when what is behind it does. The wiki page Dev Deploying has the reasons.
  *
  * The site has a seq and host of its own, because the memory caches are shared by every spec in
  * the JVM and keyed by site. */
class PublicAssetVersionSpec extends PlaySpec with GuiceOneAppPerSuite with BeforeAndAfterAll {

  private val dbName = TestApplication.randomDbName("public_asset")
  private val host = "public-asset.test"

  override def fakeApplication(): Application =
    GuiceApplicationBuilder()
      .configure(TestApplication.baseConfiguration(dbName))
      .overrides(bind[SyncCacheApi].toInstance(new TestApplication.TestSyncCacheApi))
      .build()

  override def beforeAll(): Unit = {
    super.beforeAll()
    app.injector.instanceOf[play.api.db.Database].withConnection { implicit connection =>
      TestSchema.createAll()
      Seq(
        "INSERT INTO Site (seq, name, abbr, mainDomain) VALUES (59, 'PublicAsset', 'PublicAsset', 'public-asset.test')",
        "INSERT INTO SiteDomain (site, domain) VALUES (59, 'public-asset.test')",
        "INSERT INTO Permission (site, target, targetType, actor, actorType, action) VALUES (59, '', 'All', '', 'All', 1)",
      ).foreach(sql => SQL(sql).execute())
    }
    TestApplication.resetMemoryCaches()
  }

  override def afterAll(): Unit = {
    TestApplication.resetMemoryCaches()
    super.afterAll()
  }

  /** (path, v) for every CSS and JS file under /public/ the page names; v is None when it has none. */
  private val ownAsset = """(?:href|src)="/public/([^"?]+\.(?:css|js))(?:\?v=([^"]*))?"""".r

  "a page view" should {
    "name each of its own CSS and JS files at an address carrying the digest of what is served there" in {
      val html = contentAsString(route(app, FakeRequest(GET, "/w/FrontPage").withHeaders(HOST -> host)).get)
      val named = ownAsset.findAllMatchIn(html).map(m => m.group(1) -> Option(m.group(2))).toSeq

      named.map(_._1) must contain allOf ("wiki.css", "js/js.js")
      named.collect { case (file, None) => file } mustBe empty
      named.collect { case (file, Some(v)) => file -> v }.foreach { case (file, v) =>
        val served = route(app, FakeRequest(GET, s"/public/$file?v=$v")).get
        status(served) mustBe OK
        // An asset is a streamed body, which the default NoMaterializer cannot read.
        PublicAsset.version(contentAsBytes(served)(defaultAwaitTimeout, app.materializer).toArray) mustBe v
      }
    }
  }

  "a file that is not there" should {
    "keep its bare address, so the page still renders and the browser gets the 404 it always would" in {
      PublicAsset.url("no-such-file.css") mustBe "/public/no-such-file.css"
    }
  }
}
