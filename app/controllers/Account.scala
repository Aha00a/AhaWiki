package controllers

import org.apache.pekko.actor.ActorRef
import org.apache.pekko.actor.ActorSystem
import logics.AhaWikiCache
import logics.ApplicationConf
import logics.SessionLogic
import logics.SiteLogic
import models.ContextSite
import models.tables.Site
import models.tables.User
import models.tables.UserEmail
import models.tables.UserMerge
import play.api.db.Database
import play.api.libs.ws.WSClient
import play.api.mvc._

import javax.inject.Inject
import javax.inject.Named
import scala.concurrent.ExecutionContext

class Account @Inject()(
  implicit val
  controllerComponents: ControllerComponents,
  actorSystem: ActorSystem,
  database: Database,
  @Named("db-actor") actorAhaWiki: ActorRef,
  applicationConf: ApplicationConf,
  ahaWikiCache: AhaWikiCache,
  wsClient: WSClient,
  executionContext: ExecutionContext,
) extends BaseController {

  def settings(): Action[AnyContent] = Action { implicit request =>
    SessionLogic.getUser(request) match {
      case None =>
        Redirect(routes.Auth.login).flashing("error" -> "Login required.")
      case Some(user) =>
        database.withConnection { implicit connection =>
          implicit val site: Site = SiteLogic.get(request.host)
          implicit val contextSite: ContextSite = ContextSite()
          Ok(views.html.Account.settings(user, UserEmail.selectByUser(user.seq)))
        }
    }
  }

  def mergeEmail(email: String): Action[AnyContent] = Action { implicit request =>
    SessionLogic.getUser(request) match {
      case None =>
        Redirect(routes.Auth.login).flashing("error" -> "Login required.")
      case Some(user) =>
        database.withConnection { implicit connection =>
          if (!request.session.get(SessionLogic.sessionKeyPendingMergeEmail).contains(email)) {
            Redirect(routes.Account.settings()).flashing("error" -> "Google email verification is required before merge.")
          } else UserEmail.selectByEmail(email) match {
            case Some(existing) if existing.user == user.seq =>
              Redirect(routes.Account.settings())
                .withSession(request.session - SessionLogic.sessionKeyPendingMergeEmail)
                .flashing("success" -> "Email is already connected.")
            case Some(existing) =>
              implicit val site: Site = SiteLogic.get(request.host)
              implicit val contextSite: ContextSite = ContextSite()
              Ok(views.html.Account.mergeEmail(email, User.selectBySeq(existing.user)))
            case None =>
              Redirect(routes.Account.settings()).flashing("error" -> "Email is not connected to another account.")
          }
        }
    }
  }

  def confirmMergeEmail(): Action[AnyContent] = Action { implicit request =>
    val email = request.body.asFormUrlEncoded
      .flatMap(_.get("email").flatMap(_.headOption))
      .map(_.trim)
      .getOrElse("")

    SessionLogic.getUser(request) match {
      case None =>
        Redirect(routes.Auth.login).flashing("error" -> "Login required.")
      case Some(user) if email.isEmpty =>
        Redirect(routes.Account.settings()).flashing("error" -> "Email is required.")
      case Some(user) =>
        database.withConnection { implicit connection =>
          if (!request.session.get(SessionLogic.sessionKeyPendingMergeEmail).contains(email)) {
            Redirect(routes.Account.settings()).flashing("error" -> "Google email verification is required before merge.")
          } else UserEmail.selectByEmail(email) match {
            case Some(existing) if existing.user == user.seq =>
              Redirect(routes.Account.settings())
                .withSession(request.session - SessionLogic.sessionKeyPendingMergeEmail)
                .flashing("success" -> "Email is already connected.")
            case Some(existing) =>
              UserMerge.mergeInto(canonicalUser = user.seq, duplicateUser = existing.user)
              Redirect(routes.Account.settings())
                .withSession(request.session - SessionLogic.sessionKeyPendingMergeEmail)
                .flashing("success" -> "Accounts were merged.")
            case None =>
              Redirect(routes.Account.settings()).flashing("error" -> "Email is not connected to another account.")
          }
        }
    }
  }
}
