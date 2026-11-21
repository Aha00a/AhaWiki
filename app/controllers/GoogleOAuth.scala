package controllers

import com.aha00a.play.Implicits._
import com.aha00a.play.utils.GoogleOAuthApi
import logics.AhaWikiCacheMemoryDomainSite
import logics.ApplicationConf
import logics.FirstLoginNotice
import logics.OAuthHandoff
import logics.SessionLogic
import models.tables.User
import models.tables.UserEmail
import play.api.Logging
import play.api.db.Database
import play.api.libs.crypto.CookieSigner
import play.api.libs.ws.WSClient
import play.api.mvc._

import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

/**
 * Logging in with Google.
 *
 * Every login leaves for Google with a signed `state` and comes back with it, and is finished only
 * in the browser that started it: the state names the host and holds the digest of a secret that
 * browser was given in a cookie when it left. A callback with no state, with a state of another
 * host, or in a browser without the cookie is refused before Google is asked anything.
 *
 * Until 2026-11-21 only a site under the parent domain did this (OAuthHandoff says why those go
 * through the parent host). Every other host -- the parent itself, a site on a domain of its own,
 * every host when no parent domain is set -- sent no state, redeemed whatever code a callback
 * brought, and took where to go and whether this was «connect an e-mail» from the flash. So a link
 * to the callback carrying someone else's code signed its visitor in as that someone; with a
 * connect pending in the visitor's flash it joined that someone's e-mail to the visitor's account;
 * and the address a login returned to was the Referer as sent, another site's included.
 */
//noinspection TypeAnnotation
class GoogleOAuth @Inject()(
  implicit val
  controllerComponents: ControllerComponents,
  database: Database,
  wsClient: WSClient,
  executionContext: ExecutionContext,
  applicationConf: ApplicationConf,
  cookieSigner: CookieSigner,
) extends BaseController with Logging {
  private val confApi = applicationConf.AhaWiki.google.credentials.oAuth
  // What a sealed value is for, so that one cannot be used as another: a state the parent host
  // relays for a site under it, a state the host that left for Google finishes itself, and the
  // profile the parent hands to a site.
  private val purposeState = "state"
  private val purposeDirect = "direct"
  private val purposeHandoff = "handoff"

  // The browser's half of the check has a cookie of its own. For the handoff it was kept in the
  // flash until 2026-11-21, but the flash lasts until the next request this browser makes to this
  // host, whichever request that is -- and another tab of the wiki can make one while this tab is
  // away at Google. The login would then end in «Login expired» for no reason the person could see.
  // Lax, so that it comes back on the navigation from Google; five minutes, as long as the state.
  //
  // It holds a secret, and the state only its digest (OAuthHandoff.nonceOf), so an address read in
  // a log or a history does not say what the cookie must hold. Over https its name carries the
  // `__Host-` prefix: a browser then takes it only from this very host, Secure and on `/`, so a page
  // on another host under the same parent domain cannot plant one -- and a page under the parent
  // can run script, since an author may write HTML.
  private def nonceCookieName(implicit request: RequestHeader): String =
    if (request.secure) "__Host-oauthNonce" else "oauthNonce"
  private def nonceCookie(secret: String)(implicit request: RequestHeader): Cookie = Cookie(
    nonceCookieName, secret,
    maxAge = Some((OAuthHandoff.MaxAgeMillis / 1000).toInt),
    path = "/",
    domain = None,
    secure = request.secure,
    httpOnly = true,
    sameSite = Some(Cookie.SameSite.Lax),
  )
  private def discardNonce(implicit request: RequestHeader) =
    DiscardingCookie(nonceCookieName, path = "/", secure = request.secure)

  /** This value was sealed for this host, and this browser is the one that left with it. */
  private def startedHere(value: OAuthHandoff.Value)(implicit request: RequestHeader): Boolean =
    value.host == request.host && request.cookies.get(nonceCookieName).exists(cookie => OAuthHandoff.nonceOf(cookie.value) == value.nonce)

  // Every refusal is this redirect, not a 400. An error answer is written to the access log and
  // the AccessLog table with its address, and a callback's address holds Google's code, which
  // until it is redeemed can be redeemed by anyone who reads it (FilterAccessLog also drops the
  // query of these paths now). A redirect is not stored at all.
  private def expired: Result = Redirect("/").flashing("error" -> "Login expired. Please try again.")

  def googleApiRedirectUri()(implicit request: Request[Any]): String = {
    routes.GoogleOAuth.callback("").absoluteURL().replace("?code=", "")
  }

  private def googleOAuthUrl(redirectUri: String): String =
    "https://accounts.google.com/o/oauth2/auth?response_type=code&scope=https://www.googleapis.com/auth/userinfo.email https://www.googleapis.com/auth/userinfo.profile&client_id=" + confApi.clientId() + "&redirect_uri=" + redirectUri

  private def parentDomain: String = applicationConf.AhaWiki.siteRequest.parentDomain()
  private def hubRedirectUri: String = s"https://$parentDomain" + routes.GoogleOAuth.callback("").url.replace("?code=", "")
  private def sign(text: String): String = cookieSigner.sign(text)

  /**
   * Google's answer for a code: the e-mail and the picture. A member so that a spec can answer in
   * its place -- it is the one step of a login that needs Google.
   */
  protected def retrieveProfile(code: String, redirectUri: String): Future[Option[(String, Option[String])]] =
    GoogleOAuthApi().retrieveProfileWithCode(code, confApi.clientId(), confApi.clientSecret(), redirectUri)
      .map(_.map(profile => (profile.email, profile.profileImageUrl)))

  /** Off to Google, to come straight back to this host or, under the parent domain, through it. See OAuthHandoff. */
  private def startOAuth(redirect: String, connect: Boolean)(implicit request: Request[Any]): Result = {
    val viaParent = OAuthHandoff.isUnderParent(request.host, parentDomain)
    val secret = java.util.UUID.randomUUID().toString
    val state = OAuthHandoff.seal(OAuthHandoff.Value(
      if (viaParent) purposeState else purposeDirect,
      request.host, OAuthHandoff.localPath(redirect), OAuthHandoff.nonceOf(secret), connect, System.currentTimeMillis() + OAuthHandoff.MaxAgeMillis, "", ""
    ), sign)
    Redirect(googleOAuthUrl(if (viaParent) hubRedirectUri else googleApiRedirectUri()) + "&state=" + state)
      .withCookies(nonceCookie(secret))
  }

  def login = Action { implicit request =>
    startOAuth(request.refererOrRoot, connect = false)
  }

  def connect = Action { implicit request =>
    if (SessionLogic.getUser(request).isEmpty) {
      Redirect(routes.Auth.login).flashing("error" -> "Login required.")
    } else {
      startOAuth(routes.Account.settings().url, connect = true)
    }
  }

  /** On the parent host: read the profile, and send it to the site that asked. */
  private def relayToSite(code: String, state: OAuthHandoff.Value): Future[Result] =
    retrieveProfile(code, hubRedirectUri).map { profileOpt =>
      val token = OAuthHandoff.seal(state.copy(
        purpose = purposeHandoff,
        expires = System.currentTimeMillis() + OAuthHandoff.MaxAgeMillis,
        email = profileOpt.map(_._1).getOrElse(""),
        profileImageUrl = profileOpt.flatMap(_._2).getOrElse(""),
      ), sign)
      Redirect(s"https://${state.host}" + routes.GoogleOAuth.handoff(token).url)
    }

  def callback(code: String): Action[AnyContent] = Action.async { implicit request =>
    val now = System.currentTimeMillis()
    request.getQueryString("state") match {
      // No state: nothing here says which browser asked for this login, or that any did.
      case None => Future.successful(expired)
      case Some(token) =>
        OAuthHandoff.open(token, purposeState, now, sign) match {
          // For a site under the parent, on the parent host. The state is ours, but it named a
          // host a request claimed: only a real site gets a profile. The browser is checked where
          // the profile lands, in `handoff`.
          case Some(state) if OAuthHandoff.isUnderParent(state.host, parentDomain) && AhaWikiCacheMemoryDomainSite.get(state.host).isDefined =>
            relayToSite(code, state)
          case Some(_) =>
            Future.successful(expired)
          case None =>
            OAuthHandoff.open(token, purposeDirect, now, sign).filter(startedHere) match {
              case Some(state) =>
                retrieveProfile(code, googleApiRedirectUri())
                  .map(profileOpt => complete(profileOpt, state.redirect, state.connect).discardingCookies(discardNonce))
              case None =>
                Future.successful(expired)
            }
        }
    }
  }

  /** On the site: the profile the parent host read, for the browser that started this login. */
  def handoff(token: String): Action[AnyContent] = Action { implicit request =>
    OAuthHandoff.open(token, purposeHandoff, System.currentTimeMillis(), sign)
      .filter(startedHere)
      .map(value => complete(
        Some(value.email).filter(_.nonEmpty).map(email => (email, Some(value.profileImageUrl).filter(_.nonEmpty))),
        value.redirect,
        value.connect,
      ).discardingCookies(discardNonce))
      .getOrElse(expired)
  }

  private def complete(profileOpt: Option[(String, Option[String])], redirectUrl: String, isConnect: Boolean)(implicit request: Request[AnyContent]): Result = {
    if (isConnect) {
      profileOpt match {
        case Some((email, _)) =>
          SessionLogic.getUser(request) match {
            case None =>
              Redirect(routes.Auth.login).flashing("error" -> "Login required.")
            case Some(currentUser) =>
              database.withConnection { implicit connection =>
                UserEmail.selectByEmail(email) match {
                  case None =>
                    val isPrimary = !UserEmail.hasPrimary(currentUser.seq)
                    UserEmail.insert(currentUser.seq, email, isPrimary)
                    Redirect(redirectUrl).flashing("success" -> "Email connected.")
                  case Some(existing) if existing.user == currentUser.seq =>
                    Redirect(redirectUrl).flashing("success" -> "Email is already connected.")
                  case Some(_) =>
                    Redirect(routes.Account.mergeEmail(email))
                      .withSession(request.session + (SessionLogic.sessionKeyPendingMergeEmail -> email))
                      .flashing("warning" -> "Email is connected to another account.")
                }
              }
          }
        case None =>
          Redirect(redirectUrl).flashing("error" -> "Google account connection failed.")
      }
    } else {
      profileOpt.flatMap { case (email, profileImageUrl) =>
        database.withConnection { implicit connection =>
          User.selectOrInsert(email, profileImageUrl)
            .map(user => (user, profileImageUrl))
        }
      }
        .map { case (loggedIn, profileImageUrl) =>
          // Said once, on the login that made the account: the name it was given is public.
          val notice = Some(loggedIn.user.nickname).filter(_ => loggedIn.isNew)
            .map(nickname => "info" -> FirstLoginNotice.html(nickname, routes.Account.settings().url))
          Redirect(redirectUrl)
            .withSession(SessionLogic.login(request, loggedIn.user, profileImageUrl))
            .flashing(Seq("success" -> "Successfully logged in.") ++ notice: _*)
        }
        .getOrElse {
          Redirect(redirectUrl)
            .withNewSession
            .flashing("error" -> "User creation failed")
        }
    }
  }
}
