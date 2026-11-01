package controllers

import com.aha00a.play.Implicits._
import com.aha00a.play.utils.GoogleOAuthApi
import logics.AhaWikiCacheMemoryDomainSite
import logics.ApplicationConf
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
  private val purposeState = "state"
  private val purposeHandoff = "handoff"
  private val flashKeyNonce = "oauthNonce"

  def googleApiRedirectUri()(implicit request: Request[Any]): String = {
    routes.GoogleOAuth.callback("").absoluteURL().replace("?code=", "")
  }

  private def googleOAuthUrl(redirectUri: String): String =
    "https://accounts.google.com/o/oauth2/auth?response_type=code&scope=https://www.googleapis.com/auth/userinfo.email https://www.googleapis.com/auth/userinfo.profile&client_id=" + confApi.clientId() + "&redirect_uri=" + redirectUri

  private def parentDomain: String = applicationConf.AhaWiki.siteRequest.parentDomain()
  private def hubRedirectUri: String = s"https://$parentDomain" + routes.GoogleOAuth.callback("").url.replace("?code=", "")
  private def sign(text: String): String = cookieSigner.sign(text)

  /** Off to Google, either the plain way or, under the parent domain, through it. See OAuthHandoff. */
  private def startOAuth(redirect: String, flash: (String, String)*)(implicit request: Request[Any]): Result = {
    val connect = flash.contains("oauthMode" -> "connect")
    if (OAuthHandoff.isUnderParent(request.host, parentDomain)) {
      val nonce = java.util.UUID.randomUUID().toString
      val state = OAuthHandoff.seal(OAuthHandoff.Value(
        purposeState, request.host, OAuthHandoff.localPath(redirect), nonce, connect, System.currentTimeMillis() + OAuthHandoff.MaxAgeMillis, "", ""
      ), sign)
      Redirect(googleOAuthUrl(hubRedirectUri) + "&state=" + state).flashing(flashKeyNonce -> nonce)
    } else {
      Redirect(googleOAuthUrl(googleApiRedirectUri())).flashing(flash: _*)
    }
  }

  def login = Action { implicit request =>
    startOAuth(request.refererOrRoot, "redirect" -> request.refererOrRoot)
  }

  def connect = Action { implicit request =>
    if (SessionLogic.getUser(request).isEmpty) {
      Redirect(routes.Auth.login).flashing("error" -> "Login required.")
    } else {
      startOAuth(routes.Account.settings().url, "redirect" -> routes.Account.settings().url, "oauthMode" -> "connect")
    }
  }

  /** On the parent host: read the profile, and send it to the site that asked. */
  private def relayToSite(code: String, state: OAuthHandoff.Value): Future[Result] =
    GoogleOAuthApi().retrieveProfileWithCode(code, confApi.clientId(), confApi.clientSecret(), hubRedirectUri).map { profileOpt =>
      val token = OAuthHandoff.seal(state.copy(
        purpose = purposeHandoff,
        expires = System.currentTimeMillis() + OAuthHandoff.MaxAgeMillis,
        email = profileOpt.map(_.email).getOrElse(""),
        profileImageUrl = profileOpt.flatMap(_.profileImageUrl).getOrElse(""),
      ), sign)
      Redirect(s"https://${state.host}" + routes.GoogleOAuth.handoff(token).url)
    }

  def callback(code: String): Action[AnyContent] = Action.async { implicit request =>
    request.getQueryString("state").map { token =>
      OAuthHandoff.open(token, purposeState, System.currentTimeMillis(), sign)
        // The state is ours, but it named a host a request claimed. Only a real site gets a profile.
        .filter(state => OAuthHandoff.isUnderParent(state.host, parentDomain) && AhaWikiCacheMemoryDomainSite.get(state.host).isDefined)
        .map(state => relayToSite(code, state))
        .getOrElse(Future.successful(BadRequest("Invalid login state.")))
    }.getOrElse {
      val api = GoogleOAuthApi()
      api.retrieveProfileWithCode(code, confApi.clientId(), confApi.clientSecret(), googleApiRedirectUri()).map { profileOpt =>
        complete(profileOpt.map(profile => (profile.email, profile.profileImageUrl)), request.flash.get("redirect").getOrElse("/"), request.flash.get("oauthMode").contains("connect"))
      }
    }
  }

  /** On the site: the profile the parent host read, for the browser that started this login. */
  def handoff(token: String): Action[AnyContent] = Action { implicit request =>
    OAuthHandoff.open(token, purposeHandoff, System.currentTimeMillis(), sign)
      .filter(value => value.host == request.host && request.flash.get(flashKeyNonce).contains(value.nonce))
      .map(value => complete(
        Some(value.email).filter(_.nonEmpty).map(email => (email, Some(value.profileImageUrl).filter(_.nonEmpty))),
        value.redirect,
        value.connect,
      ))
      .getOrElse(Redirect("/").flashing("error" -> "Login expired. Please try again."))
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
        .map { case (user, profileImageUrl) =>
          Redirect(redirectUrl)
            .withSession(SessionLogic.login(request, user, profileImageUrl))
            .flashing("success" -> "Successfully logged in.")
        }
        .getOrElse {
          Redirect(redirectUrl)
            .withNewSession
            .flashing("error" -> "User creation failed")
        }
    }
  }
}
