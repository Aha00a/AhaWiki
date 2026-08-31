package com.aha00a.controllers

import anorm.SQL
import com.aha00a.tests.TestApplication
import com.aha00a.tests.TestSchema
import logics.SessionLogic
import org.scalatest.BeforeAndAfterAll
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Application
import play.api.cache.SyncCacheApi
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.FakeRequest
import play.api.test.Helpers._

import java.sql.DriverManager

/**
 * The account page renders, with both of its sections in it.
 *
 * A Twirl template that does not compile fails the build, but one that compiles and then
 * throws while rendering — a null the view dereferences, a missing implicit filled in with
 * something empty — fails only in a browser. This is the cheapest thing that opens the page.
 *
 * `AHAWIKI_RENDER_DUMP` writes the rendered HTML there, which is how the markup gets looked
 * at without a local database to run the real server against.
 */
class AccountSettingsRenderSpec extends PlaySpec with GuiceOneAppPerSuite with BeforeAndAfterAll {

  private val dbName = TestApplication.randomDbName("account_render")
  private val aliceSeq = 10L

  override def fakeApplication(): Application = {
    GuiceApplicationBuilder()
      .configure(TestApplication.baseConfiguration(dbName))
      .overrides(bind[SyncCacheApi].toInstance(new TestApplication.TestSyncCacheApi))
      .build()
  }

  override def beforeAll(): Unit = {
    super.beforeAll()
    Class.forName("org.h2.Driver")
    val connection = DriverManager.getConnection(TestApplication.h2Url(dbName), "sa", "")
    TestSchema.createAll()(connection)
    Seq(
      "INSERT INTO Site (seq, name, abbr) VALUES (1, 'SiteA', 'SiteA')",
      s"INSERT INTO `User` (seq, nickname) VALUES ($aliceSeq, 'alicewiki')",
      s"INSERT INTO UserEmail (`user`, email, isPrimary) VALUES ($aliceSeq, 'alice@example.com', 1)",
    ).foreach(sql => SQL(sql).execute()(connection))
    connection.close()
    TestApplication.resetMemoryCaches()
  }

  "GET /account" should {
    "render both the nickname section and the API key section" in {
      val result = route(app, FakeRequest(GET, "/account").withSession(
        SessionLogic.sessionKeySeq -> aliceSeq.toString,
        SessionLogic.sessionKeyNickname -> "alicewiki",
        SessionLogic.sessionKeyLoginEmail -> "alice@example.com",
      )).get

      status(result) mustBe OK
      val html = contentAsString(result)

      Option(System.getenv("AHAWIKI_RENDER_DUMP")).filter(_.nonEmpty).foreach { path =>
        java.nio.file.Files.write(java.nio.file.Paths.get(path), html.getBytes("UTF-8"))
      }

      html must include("accountNickname")
      html must include("data-nickname-form")
      html must include("data-nickname-cancel")
      html must include("accountApiKeys")
      // The shared helpers were lifted out of the API-key block; both sections now call them,
      // so exactly one copy should be in the page.
      html.sliding("function escapeHtml".length).count(_ == "function escapeHtml") mustBe 1
    }
  }
}
