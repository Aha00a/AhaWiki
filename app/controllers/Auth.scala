package controllers

import com.aha00a.commons.implicits.Implicits._
import play.api.mvc._


class Auth extends Controller {
  def login = Action { implicit request =>
    Redirect(routes.GoogleOAuth.login())
  }

  def logout = Action { implicit request =>
    Redirect(request.refererOrRoot).withNewSession.flashing("success" -> "Successfully logged out.")
  }
}



