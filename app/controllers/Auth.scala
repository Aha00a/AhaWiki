package controllers

import logics.{ApplicationConf, SessionLogic}
import models.WikiContext
import play.api.Routes
import play.api.data.Form
import play.api.data.Forms._
import play.api.mvc._
import com.aha00a.commons.utils.RequestUtil


class Auth extends Controller {
  def login = Action { implicit request =>
    Redirect(routes.GoogleOAuth.login())
  }

  def logout = Action { implicit request =>
    Redirect(RequestUtil.refererOrRoot(request)).withNewSession.flashing("success" -> "Successfully logged out.")
  }
}



