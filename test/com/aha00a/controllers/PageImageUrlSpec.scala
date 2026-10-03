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
import play.api.libs.json.JsArray
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers._

/** A page's image as the adjacent-pages graph (/api/links) and a page preview (/api/pagePreview)
  * hand it to the browser. They answer from the same function now (Api.pageImageUrl).
  *
  * The graph used to have its own copy, which turned `/public/x.png` into
  * `https://<host>//public/x.png`. nginx merged the doubled slash until 2026-10-04; after that the
  * address was a 404.
  *
  * A fake request is plain http, so the expected addresses are too; behind the proxy the scheme
  * comes from X-Forwarded-Proto. The site has a seq and host of its own, because the memory caches
  * are shared by every spec in the JVM and keyed by site. */
class PageImageUrlSpec extends PlaySpec with GuiceOneAppPerSuite with BeforeAndAfterAll {

  private val host = "page-image-url.test"

  override def fakeApplication(): Application =
    GuiceApplicationBuilder()
      .configure(TestApplication.baseConfiguration(TestApplication.randomDbName("page_image_url")))
      .overrides(bind[SyncCacheApi].toInstance(new TestApplication.TestSyncCacheApi))
      .build()

  // Page name -> what PageMeta.image holds for it.
  private val images = Seq(
    "Full" -> "https://cdn.test/full.png",
    "RootRelative" -> "/public/root.png",
    "ProtocolRelative" -> "//cdn.test/protocol.png",
    "Relative" -> "relative/rel.png",
  )

  override def beforeAll(): Unit = {
    super.beforeAll()
    app.injector.instanceOf[play.api.db.Database].withConnection { implicit connection =>
      TestSchema.createAll()
      Seq(
        s"INSERT INTO Site (seq, name, abbr, mainDomain) VALUES (62, 'PageImageUrl', 'PageImageUrl', '$host')",
        s"INSERT INTO SiteDomain (site, domain) VALUES (62, '$host')",
        "INSERT INTO Permission (site, target, targetType, actor, actorType, action) VALUES (62, '', 'All', '', 'All', 255)",
        "INSERT INTO `User` (seq, nickname) VALUES (62, 'pageimage')",
        "INSERT INTO Page (site, name, revision, dateTime, `user`, remoteAddress, comment, content) VALUES (62, 'Hub', 1, NOW(), 62, '127.0.0.1', '', 'hub')",
        "INSERT INTO PageMeta (site, name, revision) VALUES (62, 'Hub', 1)",
      ).foreach(sql => SQL(sql).execute())
      images.foreach { case (name, image) =>
        SQL("INSERT INTO Page (site, name, revision, dateTime, `user`, remoteAddress, comment, content) VALUES (62, {name}, 1, NOW(), 62, '127.0.0.1', '', {name})")
          .on("name" -> name).execute()
        SQL("INSERT INTO PageMeta (site, name, revision, image) VALUES (62, {name}, 1, {image})")
          .on("name" -> name, "image" -> image).execute()
        SQL("INSERT INTO CalculatedLink (site, src, dst, alias) VALUES (62, 'Hub', {name}, '')")
          .on("name" -> name).execute()
      }
    }
    TestApplication.resetMemoryCaches()
    logics.AhaWikiCacheMemoryDomainSite.refresh()(app.injector.instanceOf[play.api.db.Database])
  }

  override def afterAll(): Unit = {
    TestApplication.resetMemoryCaches()
    super.afterAll()
  }

  private val expected = Map(
    "Full" -> "https://cdn.test/full.png",
    "RootRelative" -> s"http://$host/public/root.png",
    "ProtocolRelative" -> "http://cdn.test/protocol.png",
    "Relative" -> s"http://$host/relative/rel.png",
  )

  "/api/links" should {
    "give each adjacent page's image as an address the browser can load, without a doubled slash" in {
      val result = route(app, FakeRequest(GET, "/api/links/Hub").withHeaders("Host" -> host)).get
      status(result) mustBe OK
      val byPage = contentAsJson(result).as[JsArray].value.map(link => (link \ "dst").as[String] -> (link \ "imageUrl").as[String]).toMap
      byPage mustBe expected
    }
  }

  "/api/pagePreview" should {
    "give the same address for the same image" in {
      expected.foreach { case (name, url) =>
        val result = route(app, FakeRequest(GET, s"/api/pagePreview/$name").withHeaders("Host" -> host)).get
        status(result) mustBe OK
        (contentAsJson(result) \ "image").as[String] mustBe url
      }
    }
  }
}
