package com.aha00a.controllers

import anorm.SQL
import com.aha00a.tests.TestApplication
import com.aha00a.tests.TestSchema
import models.WikiActors
import org.apache.pekko.actor.ActorSystem
import org.scalatest.BeforeAndAfterAll
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Application
import play.api.cache.SyncCacheApi
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.FakeRequest
import play.api.test.Helpers._

/** A `#!redirect` page, and the messages a request arrives with.
  *
  * A flash message lives for one request, and a redirecting page is one more hop. So the redirect
  * has to hand on what it was given, and until 2026-11-14 it handed on only `success`: an `error`,
  * an `info` or a `warning` that arrived at a redirecting page was never shown.
  *
  * A login is where that bites. One started without a referer comes back to `/`, which is
  * FrontPage, which on a wiki like ahawiki.net is a redirect -- so a login that failed could not
  * say so there: "Login expired. Please try again." is an `error`. What a newcomer is told about
  * their name (FirstLoginNotice) is an `info`, and making that is how this was found.
  *
  * The site has a seq and host of its own, because the memory caches are shared by every spec in
  * the JVM and keyed by site. */
class WikiRedirectFlashSpec extends PlaySpec with GuiceOneAppPerSuite with BeforeAndAfterAll {

  private val dbName = TestApplication.randomDbName("redirect_flash")
  private val host = "redirect-flash.test"
  private val actorSystem = ActorSystem(s"$dbName-actors")

  override def fakeApplication(): Application =
    GuiceApplicationBuilder()
      .configure(TestApplication.baseConfiguration(dbName))
      .overrides(
        bind[SyncCacheApi].toInstance(new TestApplication.TestSyncCacheApi),
        // A view now and then sends its page off to be recalculated; nothing here needs that done.
        bind[WikiActors].toInstance(WikiActors(actorSystem.deadLetters, actorSystem.deadLetters)),
      )
      .build()

  override def beforeAll(): Unit = {
    super.beforeAll()
    app.injector.instanceOf[play.api.db.Database].withConnection { implicit connection =>
      TestSchema.createAll()
      Seq(
        "INSERT INTO Site (seq, name, abbr, mainDomain) VALUES (63, 'RedirectFlash', 'RedirectFlash', 'redirect-flash.test')",
        "INSERT INTO SiteDomain (site, domain) VALUES (63, 'redirect-flash.test')",
        "INSERT INTO `User` (seq, nickname) VALUES (64, 'editor')",
        "INSERT INTO Permission (site, target, targetType, actor, actorType, action) VALUES (63, '', 'All', '', 'All', 1)",
        "INSERT INTO Page (site, name, revision, dateTime, `user`, remoteAddress, comment, content) " +
          "VALUES (63, 'FrontPage', 1, '2026-11-14 10:00:00', 64, '127.0.0.1', '', '#!redirect Hub')",
      ).foreach(sql => SQL(sql).execute())
    }
    TestApplication.resetMemoryCaches()
  }

  override def afterAll(): Unit = {
    TestApplication.resetMemoryCaches()
    actorSystem.terminate()
    super.afterAll()
  }

  private def frontPage(flash: (String, String)*) =
    route(app, FakeRequest(GET, "/w/FrontPage").withHeaders(HOST -> host).withFlash(flash: _*)).get

  "a page that redirects" should {
    "say where the reader came from" in {
      val result = frontPage()
      redirectLocation(result) mustBe Some("/w/Hub")
      flash(result).get("success").getOrElse("") must startWith("Redirected from ")
    }

    "hand on every message the request arrived with" in {
      val result = frontPage("success" -> "Successfully logged in.", "info" -> "a notice", "error" -> "an error")
      redirectLocation(result) mustBe Some("/w/Hub")
      flash(result).get("success").getOrElse("") must startWith("Successfully logged in.<br/>Redirected from ")
      flash(result).get("info") mustBe Some("a notice")
      flash(result).get("error") mustBe Some("an error")
    }
  }
}
