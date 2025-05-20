package controllers

import com.aha00a.play.Implicits._
import com.aha00a.play.utils.GoogleOAuthApi
import logics.ApplicationConf
import logics.SessionLogic
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

  def login = Action { implicit request =>
    val referer = request.refererOrRoot
    Redirect("https://accounts.google.com/o/oauth2/auth?response_type=code&scope=https://www.googleapis.com/auth/userinfo.email https://www.googleapis.com/auth/userinfo.profile&client_id=" + confApi.clientId + "&redirect_uri=" + googleApiRedirectUri)
      .flashing("redirect" -> referer)
  }

  def callback(code: String): Action[AnyContent] = Action.async { implicit request =>
    GoogleOAuthApi().retrieveEmailWithCode(code, confApi.clientId(), confApi.clientSecret(), googleApiRedirectUri) map {
      case Some(email) =>
        val user = database.withConnection { implicit connection =>
          val user = models.tables.User.selectWhereEmail(email)
          user match {
            case Some(user) =>
              user.seq
            case None =>
              val optionSeq = models.tables.User.insert(email)
              optionSeq.get
          }
        }

        Redirect(request.flash.get("redirect").getOrElse("/"))
          .withSession(SessionLogic.login(request, email, user.toInt))
          .flashing("success" -> "Successfully logged in.")
      case None =>
        Redirect(request.flash.get("redirect").getOrElse("/"))
          .withNewSession
          .flashing("error" -> "Auth Failed")
    }
  }
}


