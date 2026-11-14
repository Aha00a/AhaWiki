package com.aha00a.controllers

import anorm.SQL
import anorm.SqlParser.scalar
import com.aha00a.tests.TestApplication
import com.aha00a.tests.TestSchema
import logics.AdminLogic
import logics.OAuthHandoff
import logics.SessionLogic
import logics.SiteRequestLogic
import models.tables.SiteRequest
import org.scalatest.BeforeAndAfterAll
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Application
import play.api.cache.SyncCacheApi
import play.api.db.Database
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.crypto.CookieSigner
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import play.api.test.Helpers._

import java.sql.DriverManager

/**
 * Asking for a wiki, an admin answering, and the login a new site then depends on. The parts
 * worth pinning are the quiet ones: a site made without approval, an approval that leaves the
 * requester unable to edit, a login handed to the wrong host or the wrong browser.
 */
class ApiSiteRequestSpec extends PlaySpec with GuiceOneAppPerSuite with BeforeAndAfterAll {

  private val dbName = TestApplication.randomDbName("api_siterequest")
  private val parent = "wiki.test"
  private val aliceSeq = 10L
  private val bobSeq = 20L

  override def fakeApplication(): Application = {
    GuiceApplicationBuilder()
      .configure(TestApplication.baseConfiguration(dbName) ++ Map("AhaWiki.siteRequest.parentDomain" -> parent))
      .overrides(bind[SyncCacheApi].toInstance(new TestApplication.TestSyncCacheApi))
      .build()
  }

  private def db: Database = app.injector.instanceOf[Database]

  override def beforeAll(): Unit = {
    super.beforeAll()
    Class.forName("org.h2.Driver")
    val connection = DriverManager.getConnection(TestApplication.h2Url(dbName), "sa", "")
    TestSchema.createAll()(connection)
    Seq(
      "INSERT INTO Site (seq, name, abbr) VALUES (1, 'Parent', 'parent')",
      s"INSERT INTO SiteDomain (site, domain) VALUES (1, '$parent')",
      s"INSERT INTO `User` (seq, nickname) VALUES (${AdminLogic.SuperAdminUserSeq}, 'superadmin')",
      s"INSERT INTO `User` (seq, nickname) VALUES ($aliceSeq, 'alicewiki')",
      s"INSERT INTO `User` (seq, nickname) VALUES ($bobSeq, 'bobwiki')",
      s"INSERT INTO UserEmail (`user`, email, isPrimary) VALUES ($aliceSeq, 'alice@example.com', 1)",
    ).foreach(sql => SQL(sql).execute()(connection))
    connection.close()
    TestApplication.resetMemoryCaches()
  }

  private def as(seq: Long, nickname: String)(method: String, url: String) =
    FakeRequest(method, url).withHeaders(HOST -> parent)
      .withSession(SessionLogic.sessionKeySeq -> seq.toString, SessionLogic.sessionKeyNickname -> nickname)

  private def admin(method: String, url: String) = as(AdminLogic.SuperAdminUserSeq, "superadmin")(method, url)

  private def ask(seq: Long, nickname: String, subdomain: String, isPublic: Boolean = true) =
    route(app, as(seq, nickname)(POST, "/site/request").withFormUrlEncodedBody(
      "name" -> "My wiki", "subdomain" -> subdomain, "isPublic" -> isPublic.toString, "purpose" -> "notes",
    ).withCSRFToken).get

  private def requestOf(subdomain: String): SiteRequest =
    db.withConnection(implicit c => SiteRequest.selectAll(100).find(_.subdomain == subdomain).get)

  private def count(sql: String): Int = db.withConnection(implicit c => SQL(sql).as(scalar[Int].single))

  "an address" should {
    "be a lowercase hostname label that is not reserved" in {
      SiteRequestLogic.validateSubdomain(" My-Wiki ") mustBe Right("my-wiki")
      SiteRequestLogic.validateSubdomain("ab").isLeft mustBe true
      SiteRequestLogic.validateSubdomain("-abc").isLeft mustBe true
      SiteRequestLogic.validateSubdomain("abc-").isLeft mustBe true
      SiteRequestLogic.validateSubdomain("a.b.c").isLeft mustBe true
      SiteRequestLogic.validateSubdomain("www") mustBe Left(SiteRequestLogic.SiteRequestError.SubdomainReserved)
    }
  }

  "a request" should {
    "ask for a login, and make no site by itself" in {
      status(route(app, FakeRequest(GET, "/site/request").withHeaders(HOST -> parent)).get) mustBe OK
      status(ask(aliceSeq, "alicewiki", "alices")) mustBe SEE_OTHER
      requestOf("alices").isPending mustBe true
      count("SELECT COUNT(*) FROM Site WHERE abbr = 'alices'") mustBe 0
    }

    "be one at a time, and hold its address against others" in {
      await(ask(aliceSeq, "alicewiki", "another"))
      await(ask(bobSeq, "bobwiki", "alices"))
      count("SELECT COUNT(*) FROM SiteRequest WHERE subdomain = 'another'") mustBe 0
      count(s"SELECT COUNT(*) FROM SiteRequest WHERE `user` = $bobSeq") mustBe 0
    }
  }

  "the admin answer" should {
    "be refused to anyone else" in {
      val seq = requestOf("alices").seq
      status(route(app, as(bobSeq, "bobwiki")(POST, s"/api/Admin/SiteRequests/$seq/approve").withCSRFToken).get) must not be OK
      requestOf("alices").isPending mustBe true
    }

    "make a site the requester can administer and the public can read" in {
      val seq = requestOf("alices").seq
      status(route(app, admin(POST, s"/api/Admin/SiteRequests/$seq/approve").withCSRFToken).get) mustBe OK

      val row = requestOf("alices")
      row.status mustBe "Approved"
      val site = row.site.get
      count(s"SELECT COUNT(*) FROM SiteDomain WHERE site = $site AND domain = 'alices.$parent'") mustBe 1
      count(s"SELECT COUNT(*) FROM SiteAdmin WHERE site = $site AND `user` = $aliceSeq") mustBe 1
      count(s"SELECT action FROM Permission WHERE site = $site AND actor = 'alice@example.com'") mustBe 255
      count(s"SELECT action FROM Permission WHERE site = $site AND actorType = 'All'") mustBe 1

      // Reachable at once, through the domain cache the approval dropped.
      status(route(app, FakeRequest(GET, "/w/FrontPage").withHeaders(HOST -> s"alices.$parent")).get) mustBe OK
    }

    // The next two screens the requester sees, in the order they see them: the request page with
    // its answer, then the wiki itself. Nothing is sent to them, so these two say what to do.
    "tell the requester where the wiki is and to log in there" in {
      // The page draws the request form under the answer, and the form wants its token.
      val html = contentAsString(route(app, as(aliceSeq, "alicewiki")(GET, "/site/request").withCSRFToken).get)
      html must include(s"""<a href="https://alices.$parent/">alices.$parent</a>""")
      html must include("Log in there with this Google account")
    }

    // Nothing is sent to the requester, so the admin screen offers a letter to send by hand
    // (siteRequestMail.js). It needs the address, which only this admin answer carries.
    "tell the admin whom to write to" in {
      val rows = contentAsJson(route(app, admin(GET, "/api/Admin/SiteRequests?status=Approved")).get).as[Seq[play.api.libs.json.JsValue]]
      val row = rows.find(row => (row \ "domain").as[String] == s"alices.$parent").get
      (row \ "email").asOpt[String] mustBe Some("alice@example.com")
    }

    "open on a first page that says how to begin" in {
      val html = contentAsString(route(app, FakeRequest(GET, "/w/FrontPage").withHeaders(HOST -> s"alices.$parent")).get)
      Option(System.getenv("AHAWIKI_RENDER_DUMP")).filter(_.nonEmpty).foreach { path =>
        java.nio.file.Files.write(java.nio.file.Paths.get(path), html.getBytes("UTF-8"))
      }
      // Nobody has saved a FrontPage here, so this is the text the engine ships (conf/Page), and
      // it is written for whoever has just been handed the wiki.
      html must include("""href="https://ahawiki.net/w/GettingStarted"""")
      html must include("<code>[회의록]</code>")
    }

    "not answer twice" in {
      val seq = requestOf("alices").seq
      status(route(app, admin(POST, s"/api/Admin/SiteRequests/$seq/approve").withCSRFToken).get) mustBe BAD_REQUEST
      count("SELECT COUNT(*) FROM Site WHERE abbr = 'alices'") mustBe 1
    }

    "open nothing to the public on a private site" in {
      await(ask(aliceSeq, "alicewiki", "secret", isPublic = false))
      val seq = requestOf("secret").seq
      status(route(app, admin(POST, s"/api/Admin/SiteRequests/$seq/approve").withCSRFToken).get) mustBe OK
      count(s"SELECT COUNT(*) FROM Permission WHERE site = ${requestOf("secret").site.get} AND actorType = 'All'") mustBe 0
    }

    "turn an address already taken away, and reject with a reason" in {
      await(ask(bobSeq, "bobwiki", "alices"))
      count(s"SELECT COUNT(*) FROM SiteRequest WHERE `user` = $bobSeq") mustBe 0

      await(ask(bobSeq, "bobwiki", "bobs"))
      val seq = requestOf("bobs").seq
      status(route(app, admin(POST, s"/api/Admin/SiteRequests/$seq/reject").withFormUrlEncodedBody("rejectReason" -> "no").withCSRFToken).get) mustBe OK
      requestOf("bobs").rejectReason mustBe Some("no")
      requestOf("bobs").site mustBe None
    }
  }

  "someone logged in without permission" should {
    "be told so, and where to go" in {
      val request = FakeRequest(GET, "/w/FrontPage?action=edit").withHeaders(HOST -> s"secret.$parent")
        .withSession(SessionLogic.sessionKeySeq -> bobSeq.toString, SessionLogic.sessionKeyNickname -> "bobwiki")
      val result = route(app, request).get
      status(result) mustBe FORBIDDEN
      contentAsString(result) must include("/site/request")
    }
  }

  "an address under the parent with no site" should {
    "say so, rather than draw an empty wiki" in {
      status(route(app, FakeRequest(GET, "/w/FrontPage").withHeaders(HOST -> s"nosuch.$parent")).get) mustBe NOT_FOUND
    }
  }

  private def sign(text: String): String = app.injector.instanceOf[CookieSigner].sign(text)

  private def value(
    purpose: String = "handoff",
    host: String = s"alices.$parent",
    expires: Long = System.currentTimeMillis() + 60000,
    email: String = "alice@example.com",
  ) =
    OAuthHandoff.Value(purpose, host, "/w/Page", "nonce", connect = false, expires, email, "")

  private def handoff(token: String, host: String, nonce: String) =
    route(app, FakeRequest(GET, s"/google/oauth/handoff?token=$token").withHeaders(HOST -> host).withFlash("oauthNonce" -> nonce)).get

  "a login handed to a site" should {
    "open a session on the host it names, in the browser that started it" in {
      val result = handoff(OAuthHandoff.seal(value(), sign), s"alices.$parent", "nonce")
      redirectLocation(result) mustBe Some("/w/Page")
      session(result).get(SessionLogic.sessionKeySeq) mustBe Some(aliceSeq.toString)
    }

    "do nothing on another host, in another browser, once expired, or as a state" in {
      def refused(result: scala.concurrent.Future[play.api.mvc.Result]) =
        session(result).get(SessionLogic.sessionKeySeq) mustBe None

      refused(handoff(OAuthHandoff.seal(value(), sign), s"bobs.$parent", "nonce"))
      refused(handoff(OAuthHandoff.seal(value(), sign), s"alices.$parent", "another"))
      refused(handoff(OAuthHandoff.seal(value(expires = System.currentTimeMillis() - 1), sign), s"alices.$parent", "nonce"))
      refused(handoff(OAuthHandoff.seal(value(purpose = "state"), sign), s"alices.$parent", "nonce"))
      refused(handoff(OAuthHandoff.seal(value(), _ => "forged"), s"alices.$parent", "nonce"))
    }

    "only ever land on a path" in {
      OAuthHandoff.localPath("https://alices.wiki.test/w/Page?x=1") mustBe "/w/Page?x=1"
      OAuthHandoff.localPath("https://evil.example") mustBe "/"
      OAuthHandoff.localPath("//evil.example/") mustBe "/"
      OAuthHandoff.localPath("/\\evil.example/") mustBe "/"
    }

    "go through the parent host only from under it" in {
      OAuthHandoff.isUnderParent(s"alices.$parent", parent) mustBe true
      OAuthHandoff.isUnderParent(parent, parent) mustBe false
      OAuthHandoff.isUnderParent("example.com", parent) mustBe false
      OAuthHandoff.isUnderParent("notwiki.test", parent) mustBe false
      OAuthHandoff.isUnderParent("anything.test", "") mustBe false
    }
  }

  // The handoff is the one way into the end of a login that needs no answer from Google, so the
  // first-login notice is checked through it. The plain callback ends in the same code.
  "a first login" should {
    "say which name the newcomer was given and where to ask for another, and say it once" in {
      def login() = handoff(OAuthHandoff.seal(value(email = "new.comer@example.com"), sign), s"alices.$parent", "nonce")

      val first = login()
      session(first).get(SessionLogic.sessionKeyNickname) mustBe Some("new_comer")
      flash(first).get("success") mustBe Some("Successfully logged in.")
      val notice = flash(first).get("info").getOrElse("")
      notice must include("<strong>new_comer</strong>")
      notice must include("""href="/account#nickname"""")

      val second = login()
      session(second).get(SessionLogic.sessionKeyNickname) mustBe Some("new_comer")
      flash(second).get("success") mustBe Some("Successfully logged in.")
      flash(second).get("info") mustBe None
    }
  }

  "the login from a site under the parent" should {
    "ask Google to call the parent back, with a state only this server could sign" in {
      val result = route(app, FakeRequest(GET, "/google/oauth/login").withHeaders(HOST -> s"alices.$parent")).get
      val location = redirectLocation(result).get
      location must include(s"redirect_uri=https://$parent/google/oauth/callback")
      flash(result).get("oauthNonce") mustBe defined
      val state = location.substring(location.indexOf("&state=") + 7)
      OAuthHandoff.open(state, "state", System.currentTimeMillis(), sign).map(_.host) mustBe Some(s"alices.$parent")
    }

    "refuse a state that names no site" in {
      val token = OAuthHandoff.seal(value(purpose = "state", host = s"nosuch.$parent"), sign)
      status(route(app, FakeRequest(GET, s"/google/oauth/callback?code=x&state=$token").withHeaders(HOST -> parent)).get) mustBe BAD_REQUEST
    }
  }
}
