package controllers

import com.aha00a.play.implicits.Implicits._
import play.api.mvc._


class Auth extends Controller {
  def login: Action[AnyContent] = Action { implicit request =>
    Redirect(routes.GoogleOAuth.login())
  }

  def logout: Action[AnyContent] = Action { implicit request =>
    Redirect(request.refererOrRoot).withNewSession.flashing("success" -> "Successfully logged out.")
  }
}



