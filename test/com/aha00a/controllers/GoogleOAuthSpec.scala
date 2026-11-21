package com.aha00a.controllers

import anorm.SQL
import anorm.SqlParser.scalar
import com.aha00a.tests.TestApplication
import com.aha00a.tests.TestSchema
import logics.ApplicationConf
import logics.OAuthHandoff
import logics.SessionLogic
import org.scalatest.BeforeAndAfterAll
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Application
import play.api.cache.SyncCacheApi
import play.api.db.Database
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.crypto.CookieSigner
import play.api.libs.ws.WSClient
import play.api.mvc.ControllerComponents
import play.api.mvc.Cookie
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._

import java.net.URLDecoder
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

/**
 * A Google login, from leaving for Google to the session it ends in -- with Google's one answer
 * (which e-mail a code stands for) given by the spec, so that everything else is the real thing:
 * the state that is sealed, the cookie the browser is handed, and what the callback does with the
 * two.
 *
 * What is pinned is that a login is finished only in the browser that started it. Until
 * 2026-11-21 only a site under the parent domain was held to that (ApiSiteRequestSpec has those
 * cases, for the handoff); every other host redeemed whatever code a callback brought.
 */
class GoogleOAuthSpec extends PlaySpec with GuiceOneAppPerSuite with BeforeAndAfterAll {

  private val dbName = TestApplication.randomDbName("google_oauth")
  private val parent = "oauth.test"
  private val under = s"alices.$parent"
  private val aliceSeq = 10L
  private val bobSeq = 20L
  private val nonceCookieName = "oauthNonce"

  override def fakeApplication(): Application = {
    GuiceApplicationBuilder()
      .configure(TestApplication.baseConfiguration(dbName) ++ Map("AhaWiki.siteRequest.parentDomain" -> parent))
      .overrides(bind[SyncCacheApi].toInstance(new TestApplication.TestSyncCacheApi))
      .build()
  }

  private def db: Database = app.injector.instanceOf[Database]

  override def beforeAll(): Unit = {
    super.beforeAll()
    db.withConnection { implicit connection =>
      TestSchema.createAll()
      Seq(
        "INSERT INTO Site (seq, name, abbr, mainDomain) VALUES (1, 'Parent', 'parent', 'oauth.test')",
        "INSERT INTO Site (seq, name, abbr, mainDomain) VALUES (2, 'Alices', 'alices', 'alices.oauth.test')",
        "INSERT INTO SiteDomain (site, domain) VALUES (1, 'oauth.test')",
        "INSERT INTO SiteDomain (site, domain) VALUES (2, 'alices.oauth.test')",
        s"INSERT INTO `User` (seq, nickname) VALUES ($aliceSeq, 'alice')",
        s"INSERT INTO `User` (seq, nickname) VALUES ($bobSeq, 'bob')",
        s"INSERT INTO UserEmail (`user`, email, isPrimary) VALUES ($aliceSeq, 'alice@example.com', 1)",
        s"INSERT INTO UserEmail (`user`, email, isPrimary) VALUES ($bobSeq, 'bob@example.com', 1)",
      ).foreach(sql => SQL(sql).execute())
    }
    TestApplication.resetMemoryCaches()
  }

  override def afterAll(): Unit = {
    TestApplication.resetMemoryCaches()
    super.afterAll()
  }

  private def sign(text: String): String = app.injector.instanceOf[CookieSigner].sign(text)

  /** The controller, with `profile` as what Google says a code stands for, and a count of how often it was asked. */
  private class Watched(profile: Option[(String, Option[String])]) extends controllers.GoogleOAuth()(
    app.injector.instanceOf[ControllerComponents],
    db,
    app.injector.instanceOf[WSClient],
    app.injector.instanceOf[ExecutionContext],
    app.injector.instanceOf[ApplicationConf],
    app.injector.instanceOf[CookieSigner],
  ) {
    var asked = 0
    override protected def retrieveProfile(code: String, redirectUri: String): Future[Option[(String, Option[String])]] = {
      asked += 1
      Future.successful(profile)
    }
  }
  private def google(email: String = "alice@example.com") = new Watched(Some((email, None)))

  private def stateIn(result: Future[Result]): String = {
    val location = redirectLocation(result).getOrElse(fail("no redirect"))
    location must startWith("https://accounts.google.com/")
    """[?&]state=([^&]+)""".r.findFirstMatchIn(location).map(_.group(1)).getOrElse(fail(s"no state in $location"))
  }
  private def nonceIn(result: Future[Result]): Cookie =
    cookies(result).get(nonceCookieName).getOrElse(fail("no nonce cookie"))

  /** Leave for Google from `host`, having come from `referer`: the state in the address, and the cookie the browser keeps. */
  private def leave(controller: Watched, host: String = parent, referer: Option[String] = None, as: Option[(Long, String)] = None, connect: Boolean = false): (String, Cookie) = {
    val base = FakeRequest(GET, if (connect) "/google/oauth/connect" else "/google/oauth/login").withHeaders(Seq(HOST -> host) ++ referer.map(REFERER -> _): _*)
    val request = as.map { case (seq, nickname) => base.withSession(SessionLogic.sessionKeySeq -> seq.toString, SessionLogic.sessionKeyNickname -> nickname) }.getOrElse(base)
    val result = if (connect) controller.connect(request) else controller.login(request)
    status(result) mustBe SEE_OTHER
    (stateIn(result), nonceIn(result))
  }

  private def comeBack(controller: Watched, state: Option[String], nonce: Option[Cookie], host: String = parent, as: Option[(Long, String)] = None): Future[Result] = {
    val url = "/google/oauth/callback?code=the-code" + state.map("&state=" + _).getOrElse("")
    val withHost = FakeRequest(GET, url).withHeaders(HOST -> host)
    val withCookie = nonce.map(cookie => withHost.withCookies(cookie)).getOrElse(withHost)
    val request = as.map { case (seq, nickname) => withCookie.withSession(SessionLogic.sessionKeySeq -> seq.toString, SessionLogic.sessionKeyNickname -> nickname) }.getOrElse(withCookie)
    controller.callback("the-code")(request)
  }

  "leaving for Google" should {
    "carry a state for this host and hand the browser its nonce, on a host that finishes its own login" in {
      val result = route(app, FakeRequest(GET, "/google/oauth/login").withHeaders(HOST -> parent, REFERER -> s"http://$parent/w/Page?x=1")).get
      status(result) mustBe SEE_OTHER
      redirectLocation(result).get must include(s"redirect_uri=http://$parent/google/oauth/callback")
      val state = OAuthHandoff.open(stateIn(result), "direct", System.currentTimeMillis(), sign).getOrElse(fail("the state does not open as a direct one"))
      state.host mustBe parent
      state.redirect mustBe "/w/Page?x=1"
      state.connect mustBe false
      val cookie = nonceIn(result)
      // The state holds the digest; only the browser holds the secret.
      state.nonce mustBe OAuthHandoff.nonceOf(cookie.value)
      state.nonce must not be cookie.value
      cookie.domain mustBe None
      cookie.httpOnly mustBe true
      cookie.sameSite mustBe Some(Cookie.SameSite.Lax)
      cookie.maxAge mustBe Some((OAuthHandoff.MaxAgeMillis / 1000).toInt)
    }

    "carry a state for the parent to relay, and the same cookie, on a site under the parent" in {
      val result = route(app, FakeRequest(GET, "/google/oauth/login").withHeaders(HOST -> under, REFERER -> s"http://$under/w/Page")).get
      redirectLocation(result).get must include(s"redirect_uri=https://$parent/google/oauth/callback")
      val state = OAuthHandoff.open(stateIn(result), "state", System.currentTimeMillis(), sign).getOrElse(fail("the state does not open as one to relay"))
      state.host mustBe under
      state.redirect mustBe "/w/Page"
      OAuthHandoff.nonceOf(nonceIn(result).value) mustBe state.nonce
      // The nonce is not in the flash any more: the next request of any tab would have used it up.
      flash(result).get(nonceCookieName) mustBe None
    }

    "name the cookie so that only this very host can set it, over https" in {
      val overHttps = FakeRequest(GET, "/google/oauth/login").withHeaders(HOST -> parent)
        .withConnection(play.api.mvc.request.RemoteConnection("127.0.0.1", secure = true, None))
      val result = route(app, overHttps).get
      redirectLocation(result).get must include(s"redirect_uri=https://$parent/google/oauth/callback")
      val cookie = cookies(result).get("__Host-oauthNonce").getOrElse(fail("no __Host- cookie over https"))
      cookie.secure mustBe true
      cookie.path mustBe "/"
      cookie.domain mustBe None
      cookies(result).get(nonceCookieName) mustBe None
    }

    "not carry a path too long for the address it rides in" in {
      val long = "/w/" + ("x" * 900)
      val result = route(app, FakeRequest(GET, "/google/oauth/login").withHeaders(HOST -> parent, REFERER -> s"http://$parent$long")).get
      OAuthHandoff.open(stateIn(result), "direct", System.currentTimeMillis(), sign).get.redirect mustBe "/"
    }

    "remember only a path of this host to come back to" in {
      def comesBackTo(referer: Option[String]): String = {
        val result = route(app, FakeRequest(GET, "/google/oauth/login").withHeaders(Seq(HOST -> parent) ++ referer.map(REFERER -> _): _*)).get
        OAuthHandoff.open(stateIn(result), "direct", System.currentTimeMillis(), sign).get.redirect
      }
      comesBackTo(None) mustBe "/"
      comesBackTo(Some(s"http://$parent/w/Some%20Page?action=edit")) mustBe "/w/Some%20Page?action=edit"
      // Another site's page is not somewhere this site sends anyone, and neither is that page's path here.
      comesBackTo(Some("https://evil.example/phish")) mustBe "/"
      comesBackTo(Some(s"http://$parent.evil.example/w/Page")) mustBe "/"
      comesBackTo(Some(s"http://$parent//evil.example/")) mustBe "/"
      comesBackTo(Some("not a url")) mustBe "/"
    }
  }

  "coming back from Google" should {
    "open a session in the browser that left, and go where it came from" in {
      val controller = google()
      val (state, nonce) = leave(controller, referer = Some(s"http://$parent/w/Page?x=1"))
      val result = comeBack(controller, Some(state), Some(nonce))
      redirectLocation(result) mustBe Some("/w/Page?x=1")
      session(result).get(SessionLogic.sessionKeySeq) mustBe Some(aliceSeq.toString)
      controller.asked mustBe 1
      // The nonce has done its work.
      cookies(result).get(nonceCookieName).map(_.value) mustBe Some("")
    }

    "do nothing, and ask Google nothing, when the browser is not the one that left" in {
      val controller = google()
      val (state, nonce) = leave(controller)
      val (_, anotherNonce) = leave(controller)

      // Refused as an expired login: a redirect, which the access log does not store, rather
      // than an error answer, which it stores with the address -- and the address holds the code.
      def refused(result: Future[Result]): Unit = {
        status(result) mustBe SEE_OTHER
        redirectLocation(result) mustBe Some("/")
        flash(result).get("error") mustBe Some("Login expired. Please try again.")
        session(result).get(SessionLogic.sessionKeySeq) mustBe None
      }
      // A link someone made: a code and nothing else. This used to sign its visitor in as whoever the code stood for.
      refused(comeBack(controller, None, None))
      refused(comeBack(controller, None, Some(nonce)))
      // The state, taken from a log or a link, in a browser that never left with it.
      refused(comeBack(controller, Some(state), None))
      refused(comeBack(controller, Some(state), Some(anotherNonce)))
      refused(comeBack(controller, Some(state), Some(nonce.copy(value = "guessed"))))
      // What the state itself says the nonce is: an address read in a log gives the cookie away no more.
      val readFromTheAddress = OAuthHandoff.open(state, "direct", System.currentTimeMillis(), sign).get.nonce
      refused(comeBack(controller, Some(state), Some(nonce.copy(value = readFromTheAddress))))
      // On another host than the one it was sealed for.
      refused(comeBack(controller, Some(state), Some(nonce), host = "elsewhere.test"))
      // Not ours, or changed on the way.
      refused(comeBack(controller, Some(state.dropRight(1) + (if (state.endsWith("0")) "1" else "0")), Some(nonce)))
      refused(comeBack(controller, Some("nonsense"), Some(nonce)))
      controller.asked mustBe 0
    }

    "not disturb a session that is already there when it refuses" in {
      val controller = google()
      val result = comeBack(controller, None, None, as = Some(bobSeq -> "bob"))
      redirectLocation(result) mustBe Some("/")
      flash(result).get("error") mustBe Some("Login expired. Please try again.")
      // No new session is written, so the browser keeps the one it has.
      result.value.get.get.newSession mustBe None
      controller.asked mustBe 0
    }

    "refuse a state once its five minutes are over, and a value sealed for something else" in {
      val controller = google()
      def sealedAs(purpose: String, expires: Long) = OAuthHandoff.seal(
        OAuthHandoff.Value(purpose, parent, "/w/Page", OAuthHandoff.nonceOf("the-secret"), connect = false, expires, "", ""), sign)
      val cookie = Cookie(nonceCookieName, "the-secret")
      val soon = System.currentTimeMillis() + 60000

      session(comeBack(controller, Some(sealedAs("direct", System.currentTimeMillis() - 1)), Some(cookie))).get(SessionLogic.sessionKeySeq) mustBe None
      // A handoff is not a state, and a state for the parent's relay is not one this host finishes
      // -- nor one the relay takes for a host that is not under the parent.
      session(comeBack(controller, Some(sealedAs("handoff", soon)), Some(cookie))).get(SessionLogic.sessionKeySeq) mustBe None
      redirectLocation(comeBack(controller, Some(sealedAs("state", soon)), Some(cookie))) mustBe Some("/")
      controller.asked mustBe 0
      // And the same value, as what it is, is taken: the refusals above were about the value.
      session(comeBack(controller, Some(sealedAs("direct", soon)), Some(cookie))).get(SessionLogic.sessionKeySeq) mustBe Some(aliceSeq.toString)
    }

    "be relayed to a site under the parent without the parent opening a session" in {
      val controller = google()
      val (state, _) = leave(controller, host = under, referer = Some(s"http://$under/w/Page"))
      // Google calls the parent host back; the browser has no nonce cookie there, and needs none.
      val result = comeBack(controller, Some(state), None, host = parent)
      val location = redirectLocation(result).get
      location must startWith(s"https://$under/google/oauth/handoff?token=")
      session(result).get(SessionLogic.sessionKeySeq) mustBe None
      val token = URLDecoder.decode(location.substring(location.indexOf("token=") + 6), "UTF-8")
      val handed = OAuthHandoff.open(token, "handoff", System.currentTimeMillis(), sign).get
      handed.email mustBe "alice@example.com"
      handed.host mustBe under
      handed.redirect mustBe "/w/Page"
    }
  }

  "connecting another e-mail" should {
    "join the e-mail Google names to the account that asked, in the browser that asked" in {
      val controller = google("bob.second@example.com")
      val (state, nonce) = leave(controller, as = Some(bobSeq -> "bob"), connect = true)
      OAuthHandoff.open(state, "direct", System.currentTimeMillis(), sign).get.connect mustBe true

      val result = comeBack(controller, Some(state), Some(nonce), as = Some(bobSeq -> "bob"))
      flash(result).get("success") mustBe Some("Email connected.")
      db.withConnection { implicit connection =>
        SQL("SELECT `user` FROM UserEmail WHERE email = 'bob.second@example.com'").as(scalar[Long].singleOpt)
      } mustBe Some(bobSeq)
    }

    // What the flash made possible: bob has asked to connect, and before Google brings him back
    // someone gets his browser to open the callback with a code of theirs. «Connect» was read from
    // bob's flash, so their e-mail was joined to his account. Now connect is in the state, the
    // state is theirs or his, and neither works: theirs was not started in his browser, and his
    // goes with his own code.
    "not join an e-mail on a callback that bob's browser did not start" in {
      val attacker = google("attacker@example.com")
      val (attackersState, _) = leave(attacker, as = Some(aliceSeq -> "alice"), connect = true)
      val (_, bobsNonce) = leave(attacker, as = Some(bobSeq -> "bob"), connect = true)

      val result = comeBack(attacker, Some(attackersState), Some(bobsNonce), as = Some(bobSeq -> "bob"))
      redirectLocation(result) mustBe Some("/")
      attacker.asked mustBe 0
      db.withConnection { implicit connection =>
        SQL("SELECT COUNT(*) FROM UserEmail WHERE email = 'attacker@example.com'").as(scalar[Long].single)
      } mustBe 0
    }
  }

  "logging out" should {
    "go back to the page it came from on this host, and to no other site" in {
      def goesTo(referer: Option[String]): Option[String] =
        redirectLocation(route(app, FakeRequest(GET, "/logout").withHeaders(Seq(HOST -> parent) ++ referer.map(REFERER -> _): _*)).get)
      goesTo(Some(s"http://$parent/w/Page")) mustBe Some("/w/Page")
      goesTo(None) mustBe Some("/")
      goesTo(Some("https://evil.example/phish")) mustBe Some("/")
      // This host's name, but a path a browser reads as another host's address.
      goesTo(Some(s"http://$parent//evil.example/")) mustBe Some("/")
      goesTo(Some(s"http://$parent/\\evil.example/")) mustBe Some("/")
    }
  }
}
