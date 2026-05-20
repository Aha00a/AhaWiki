package controllers

import com.aha00a.play.Implicits._
import com.aha00a.play.utils.GoogleOAuthApi
import logics.ApplicationConf
import logics.SessionLogic
import models.tables.User
import models.tables.UserEmail
import play.api.Logging
import play.api.db.Database
import play.api.libs.ws.WSClient
import play.api.mvc._

import javax.inject.Inject
import scala.concurrent.ExecutionContext

//noinspection TypeAnnotation
class GoogleOAuth @Inject()(
  implicit val
  controllerComponents: ControllerComponents,
  database: Database,
  wsClient: WSClient,
  executionContext: ExecutionContext,
  applicationConf: ApplicationConf
) extends BaseController with Logging {
  private val confApi = applicationConf.AhaWiki.google.credentials.oAuth

  def googleApiRedirectUri()(implicit request: Request[Any]): String = {
    routes.GoogleOAuth.callback("").absoluteURL().replace("?code=", "")
  }

  private def googleOAuthUrl(redirectUri: String): String =
    "https://accounts.google.com/o/oauth2/auth?response_type=code&scope=https://www.googleapis.com/auth/userinfo.email https://www.googleapis.com/auth/userinfo.profile&client_id=" + confApi.clientId() + "&redirect_uri=" + redirectUri

  def login = Action { implicit request =>
    val referer = request.refererOrRoot
    Redirect(googleOAuthUrl(googleApiRedirectUri()))
      .flashing("redirect" -> referer)
  }

  def connect = Action { implicit request =>
    if (SessionLogic.getUser(request).isEmpty) {
      Redirect(routes.Auth.login).flashing("error" -> "Login required.")
    } else {
      Redirect(googleOAuthUrl(googleApiRedirectUri()))
        .flashing(
          "redirect" -> routes.Account.settings().url,
          "oauthMode" -> "connect"
        )
    }
  }

  def callback(code: String): Action[AnyContent] = Action.async { implicit request =>
    val redirectUrl = request.flash.get("redirect").getOrElse("/")
    val isConnect = request.flash.get("oauthMode").contains("connect")

    GoogleOAuthApi().retrieveProfileWithCode(
      code,
      confApi.clientId(),
      confApi.clientSecret(),
      googleApiRedirectUri()
    ).map { profileOpt =>
      if (isConnect) {
        profileOpt match {
          case Some(profile) =>
            SessionLogic.getUser(request) match {
              case None =>
                Redirect(routes.Auth.login).flashing("error" -> "Login required.")
              case Some(currentUser) =>
                database.withConnection { implicit connection =>
                  UserEmail.selectByEmail(profile.email) match {
                    case None =>
                      val isPrimary = !UserEmail.hasPrimary(currentUser.seq)
                      UserEmail.insert(currentUser.seq, profile.email, isPrimary)
                      Redirect(redirectUrl).flashing("success" -> "Email connected.")
                    case Some(existing) if existing.user == currentUser.seq =>
                      Redirect(redirectUrl).flashing("success" -> "Email is already connected.")
                    case Some(_) =>
                      Redirect(routes.Account.mergeEmail(profile.email))
                        .withSession(request.session + (SessionLogic.sessionKeyPendingMergeEmail -> profile.email))
                        .flashing("warning" -> "Email is connected to another account.")
                  }
                }
            }
          case None =>
            Redirect(redirectUrl).flashing("error" -> "Google account connection failed.")
        }
      } else {
        profileOpt.flatMap { profile =>
          database.withConnection { implicit connection =>
            User.selectOrInsert(profile.email, profile.profileImageUrl)
              .map(user => (user, profile.profileImageUrl))
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
}
