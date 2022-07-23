package controllers

import com.aha00a.play.Implicits._
import javax.inject.Inject
import play.api.mvc._


class Auth @Inject() (val controllerComponents: ControllerComponents) extends BaseController {
  def login: Action[AnyContent] = Action { implicit request =>
    Redirect(routes.GoogleOAuth.login)
  }

  def logout: Action[AnyContent] = Action { implicit request =>
    Redirect(request.refererOrRoot).withNewSession.flashing("success" -> "Successfully logged out.")
  }
}



