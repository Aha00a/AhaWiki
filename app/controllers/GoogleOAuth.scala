package controllers

import com.aha00a.play.Implicits._
import com.aha00a.play.utils.GoogleOAuthApi
import javax.inject.Inject
import logics.{ApplicationConf, SessionLogic}
import play.api.Configuration
import play.api.cache.CacheApi
import play.api.libs.ws.WSClient
import play.api.mvc._

import scala.concurrent.ExecutionContext

//noinspection TypeAnnotation
class GoogleOAuth @Inject()(
  implicit cacheApi: CacheApi,
  wsClient: WSClient,
  executionContext: ExecutionContext,
  configuration: Configuration
) extends Controller {
  private val confApi = ApplicationConf().AhaWiki.google.credentials.oAuth

  def googleApiRedirectUri()(implicit request: Request[Any]): String = {
    routes.GoogleOAuth.callback("").absoluteURL().replaceAllLiterally("?code=", "")
  }

  def login = Action { implicit request =>
    val referer = request.refererOrRoot
    Redirect("https://accounts.google.com/o/oauth2/auth?response_type=code&scope=https://www.googleapis.com/auth/userinfo.email&client_id=" + confApi.clientId + "&redirect_uri=" + googleApiRedirectUri)
      .flashing("redirect" -> referer)
  }

  //noinspection TypeAnnotation
  def callback(code: String) = Action.async { implicit request =>
    GoogleOAuthApi().retrieveEmailWithCode(code, confApi.clientId(), confApi.clientSecret(), googleApiRedirectUri) map {
      case Some(email) =>
        Redirect(request.flash.get("redirect").getOrElse("/"))
          .withSession(SessionLogic.login(request, email))
          .flashing("success" -> "Successfully logged in.")
      case None =>
        Redirect(request.flash.get("redirect").getOrElse("/"))
          .withNewSession
          .flashing("error" -> "Auth Failed")
    }
  }
}


