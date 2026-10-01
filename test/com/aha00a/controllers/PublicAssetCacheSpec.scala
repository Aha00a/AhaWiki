package com.aha00a.controllers

import com.aha00a.tests.TestApplication
import logics.PublicAsset
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Application
import play.api.Mode
import play.api.cache.SyncCacheApi
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.FakeRequest
import play.api.test.Helpers._

/** What /public/ tells the browser to cache when the address names a digest (controllers.PublicAssets).
  *
  * During a deploy the other instance may hold a different file under the same path, and its
  * answer must not stay in the browser's cache under an address that names other content.
  *
  * In Prod mode, because Play's Assets sends `max-age` only there; in Dev and Test it sends no
  * Cache-Control at all and Play falls back to no-cache, so every case here would pass with or
  * without the controller. */
class PublicAssetCacheSpec extends PlaySpec with GuiceOneAppPerSuite {

  override def fakeApplication(): Application =
    GuiceApplicationBuilder()
      .in(Mode.Prod)
      .configure(TestApplication.baseConfiguration(TestApplication.randomDbName("public_asset_cache")))
      .overrides(bind[SyncCacheApi].toInstance(new TestApplication.TestSyncCacheApi))
      .build()

  private def cacheControl(path: String): Option[String] = {
    val served = route(app, FakeRequest(GET, path)).get
    status(served) mustBe OK
    header(CACHE_CONTROL, served)
  }

  "/public/" should {
    "cache the file as Assets does when there is no v" in {
      cacheControl("/public/wiki.css") must (not be empty and not be Some("no-cache"))
    }

    "cache it the same way when v is the digest of the file served" in {
      cacheControl(s"/public/wiki.css?v=${PublicAsset.versionOf("wiki.css").get}") mustBe cacheControl("/public/wiki.css")
    }

    "say no-cache when v is the digest of some other content" in {
      cacheControl("/public/wiki.css?v=0000000000") mustBe Some("no-cache")
    }
  }
}
