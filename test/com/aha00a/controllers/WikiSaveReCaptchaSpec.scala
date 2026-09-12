package com.aha00a.controllers

import anorm.SQL
import com.aha00a.tests.TestApplication
import com.aha00a.tests.TestSchema
import logics.SessionLogic
import models.WikiActors
import models.tables.Page
import models.tables.Site
import org.apache.pekko.actor.ActorSystem
import org.scalatest.BeforeAndAfterAll
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Application
import play.api.cache.SyncCacheApi
import play.api.db.Database
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._

import scala.concurrent.Future

/** A web save (`POST /w/...`) carries a reCAPTCHA token, checked with Google whenever both keys
  * are configured. Until 2026-09-13 an empty token skipped the check, so a client could save
  * unchecked by leaving the token out, and Kanban did, since its page had no token to send.
  *
  * Only the answers that need no call to Google are pinned here: a missing or empty token is
  * refused before anything is sent anywhere, and with reCAPTCHA not configured a save needs
  * none. The browser's side, a new token for every save, is test/recaptcha.test.mjs and
  * test/kanban.recaptcha.test.mjs.
  *
  * Each suite has its own site seq and host, for the reason AccessControlFilteringSpec gives:
  * the permission cache is a JVM-wide singleton keyed by site seq. */
abstract class WikiSaveReCaptchaSpecBase(siteSeq: Long, host: String, reCaptchaConfiguration: Map[String, Any])
  extends PlaySpec with GuiceOneAppPerSuite with BeforeAndAfterAll {

  private val dbName = TestApplication.randomDbName(s"save$siteSeq")
  private val actorSystem = ActorSystem(s"$dbName-actors")
  protected implicit val site: Site = Site(siteSeq, "SaveWiki", "SaveWiki", host)

  override def fakeApplication(): Application =
    GuiceApplicationBuilder()
      .configure(TestApplication.baseConfiguration(dbName) ++ reCaptchaConfiguration)
      .overrides(
        bind[SyncCacheApi].toInstance(new TestApplication.TestSyncCacheApi),
        bind[WikiActors].toInstance(WikiActors(actorSystem.deadLetters, actorSystem.deadLetters)),
      )
      .build()

  private def db: Database = app.injector.instanceOf[Database]

  override def beforeAll(): Unit = {
    super.beforeAll()
    db.withConnection { implicit connection =>
      TestSchema.createAll()
      Seq(
        s"INSERT INTO Site (seq, name, abbr, mainDomain) VALUES ($siteSeq, 'SaveWiki', 'SaveWiki', '$host')",
        s"INSERT INTO SiteDomain (site, domain) VALUES ($siteSeq, '$host')",
        "INSERT INTO `User` (seq, nickname) VALUES (41, 'writer')",
        "INSERT INTO UserEmail (`user`, email, isPrimary) VALUES (41, 'writer@example.com', TRUE)",
        // Create(4) on every page, so a refused save is refused for its token and nothing else.
        s"INSERT INTO Permission (site, target, targetType, actor, actorType, action) VALUES ($siteSeq, '', 'All', 'writer@example.com', 'Exact', 4)",
      ).foreach(sql => SQL(sql).execute())
    }
    TestApplication.resetMemoryCaches()
  }

  override def afterAll(): Unit = {
    TestApplication.resetMemoryCaches()
    actorSystem.terminate()
    super.afterAll()
  }

  /** A new page, saved by a user who may create it. Minor, so no Telegram notice is attempted. */
  protected def save(name: String, recaptcha: Option[String]): Future[Result] = {
    val fields = Seq("revision" -> "0", "text" -> s"= $name\nbody", "comment" -> "test", "minorEdit" -> "true") ++
      recaptcha.map("recaptcha" -> _)
    route(app, FakeRequest(POST, s"/w/$name")
      .withHeaders(HOST -> host)
      .withSession(SessionLogic.sessionKeySeq -> "41", SessionLogic.sessionKeyNickname -> "writer")
      .withFormUrlEncodedBody(fields: _*)).get
  }

  protected def revisionOf(name: String): Option[Long] = db.withConnection { implicit connection =>
    Page.selectLastRevision(name).map(_.revision.toLong)
  }
}

class WikiSaveReCaptchaRequiredSpec extends WikiSaveReCaptchaSpecBase(9, "save-recaptcha-on.test", Map(
  "AhaWiki.google.reCAPTCHA.siteKey" -> "test-site-key",
  "AhaWiki.google.reCAPTCHA.secretKey" -> "test-secret-key",
)) {
  "POST /w/... with reCAPTCHA configured" should {
    "refuse an empty token, and save nothing" in {
      val result = save("EmptyToken", Some(""))
      status(result) mustBe FORBIDDEN
      contentAsString(result) must include("reCAPTCHA token is required")
      revisionOf("EmptyToken") mustBe None
    }

    "refuse a request with no token field the same way" in {
      val result = save("NoTokenField", None)
      status(result) mustBe FORBIDDEN
      contentAsString(result) must include("reCAPTCHA token is required")
      revisionOf("NoTokenField") mustBe None
    }
  }
}

class WikiSaveReCaptchaNotConfiguredSpec extends WikiSaveReCaptchaSpecBase(10, "save-recaptcha-off.test", Map.empty) {
  "POST /w/... with reCAPTCHA not configured" should {
    "save without a token" in {
      status(save("NoReCaptcha", Some(""))) mustBe OK
      revisionOf("NoReCaptcha") mustBe Some(1L)
    }
  }
}
