package controllers

import logics.{SessionLogic, ApplicationConf}
import play.api.{Routes, Logger}
import play.api.mvc._
import utils.{RequestUtil, GoogleApi}

import scala.concurrent.ExecutionContext.Implicits.global


class GoogleOAuth extends Controller {

  def googleApiRedirectUri()(implicit request: Request[Any]): String = {
    routes.GoogleOAuth.callback("").absoluteURL().replaceAllLiterally("?code=", "")
  }

  def login = Action { implicit request =>
    val referer = RequestUtil.refererOrRoot(request)
    Logger.error(referer)

    Redirect("https://accounts.google.com/o/oauth2/auth?response_type=code&scope=https://www.googleapis.com/auth/userinfo.email&client_id=" + ApplicationConf.AhaWiki.google.api.clientId + "&redirect_uri=" + googleApiRedirectUri)
      .flashing("redirect" -> referer)
  }

  def callback(code: String) = Action.async { implicit request =>
    GoogleApi.retrieveEmailWithCode(code, ApplicationConf.AhaWiki.google.api.clientId, ApplicationConf.AhaWiki.google.api.clientSecret, googleApiRedirectUri()).map {
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


