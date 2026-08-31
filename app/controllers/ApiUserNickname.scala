package controllers

import io.circe.Json
import logics.SessionLogic
import logics.UserNicknameChangeLogic
import models.tables.User
import models.tables.UserNicknameChangeRequest
import play.api.db.Database
import play.api.mvc._

import javax.inject._

/**
 * Nickname change requests, from both ends: a user asking, and an admin answering.
 *
 * Nothing here reads the nickname out of the session. The session's copy is a display
 * snapshot signed into a cookie on some other device a week ago, and this is the one place
 * where being a week behind would change the outcome — so the current name always comes from
 * `User`. Identity still comes from the session, but as `seq`.
 */
class ApiUserNickname @Inject()(
  implicit val
  controllerComponents: ControllerComponents,
  database: Database,
) extends BaseController with JsonResults with AdminAuth {

  private val rejectReasonMaxLength = 1000

  private def requestJson(row: UserNicknameChangeRequest, user: Option[User] = None): Json = Json.obj(
    "seq" -> Json.fromLong(row.seq),
    "user" -> Json.fromLong(row.user),
    "userNickname" -> user.map(u => Json.fromString(u.nickname)).getOrElse(Json.Null),
    "requestedNickname" -> Json.fromString(row.requestedNickname),
    "status" -> Json.fromString(row.status),
    "rejectReason" -> row.rejectReason.map(Json.fromString).getOrElse(Json.Null),
    "dateInserted" -> Json.fromString(row.dateInserted.toString),
    "dateReviewed" -> row.dateReviewed.map(v => Json.fromString(v.toString)).getOrElse(Json.Null),
  )

  /** The admin screen posts JSON and the account page posts a form, so both are accepted. */
  private def field(name: String)(implicit request: Request[AnyContent]): Option[String] =
    request.body.asJson.flatMap(json => (json \ name).asOpt[String])
      .orElse(request.body.asFormUrlEncoded.flatMap(_.get(name).flatMap(_.headOption)))
      .map(_.trim)

  private def withLoginUser(block: User.SessionUser => Result)(implicit request: RequestHeader): Result =
    SessionLogic.getUser(request).fold(JsonError(Unauthorized, "Login required."))(block)

  def accountNicknameRequests: Action[AnyContent] = Action { implicit request =>
    withLoginUser { sessionUser =>
      database.withConnection { implicit connection =>
        Ok(Json.obj(
          // The name as it is now, not as the cookie remembers it.
          "currentNickname" -> User.selectBySeq(sessionUser.seq).map(u => Json.fromString(u.nickname)).getOrElse(Json.Null),
          "pending" -> UserNicknameChangeRequest.selectPendingByUser(sessionUser.seq).map(requestJson(_)).getOrElse(Json.Null),
          "recent" -> Json.fromValues(
            UserNicknameChangeRequest.selectByUser(sessionUser.seq, UserNicknameChangeLogic.RecentRequestsPerUser).map(requestJson(_))
          ),
        ))
      }
    }
  }

  def accountRequestNickname: Action[AnyContent] = Action { implicit request =>
    withLoginUser { sessionUser =>
      database.withTransaction { implicit connection =>
        UserNicknameChangeLogic.request(sessionUser.seq, field("nickname").getOrElse("")) match {
          case Left(error) => JsonError(BadRequest, error.message)
          case Right(row) => Ok(requestJson(row))
        }
      }
    }
  }

  def accountCancelNicknameRequest(seq: Long): Action[AnyContent] = Action { implicit request =>
    withLoginUser { sessionUser =>
      database.withTransaction { implicit connection =>
        UserNicknameChangeLogic.cancel(seq, sessionUser.seq) match {
          case Left(UserNicknameChangeLogic.ReviewError.NotFound) => JsonError(NotFound, "Request not found.")
          case Left(error) => JsonError(BadRequest, error.message)
          case Right(row) => Ok(requestJson(row))
        }
      }
    }
  }

  def adminNicknameRequests(status: String): Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      AccessDenied
    } else {
      database.withConnection { implicit connection =>
        val rows = UserNicknameChangeRequest.Status.values.find(_.toString.equalsIgnoreCase(status)) match {
          case Some(value) => UserNicknameChangeRequest.selectByStatus(value)
          case None => UserNicknameChangeRequest.selectAll(UserNicknameChangeLogic.RecentRequestsPerUser * 40)
        }
        Ok(Json.fromValues(rows.map(row => requestJson(row, User.selectBySeq(row.user)))))
      }
    }
  }

  def adminApproveNicknameRequest(seq: Long): Action[AnyContent] = Action { implicit request =>
    withAdminReview(seq)((requestSeq, adminSeq, _) => connection =>
      UserNicknameChangeLogic.approve(requestSeq, adminSeq)(connection))
  }

  def adminRejectNicknameRequest(seq: Long): Action[AnyContent] = Action { implicit request =>
    withAdminReview(seq)((requestSeq, adminSeq, reason) => connection =>
      UserNicknameChangeLogic.reject(requestSeq, adminSeq, reason)(connection))
  }

  /**
   * Approve and reject differ by one call and answer identically, including which failures
   * are 404 and which are 400. Writing that twice is how the two drift into disagreeing about
   * what a missing request is.
   */
  private def withAdminReview(seq: Long)(
    review: (Long, Long, Option[String]) => java.sql.Connection => Either[UserNicknameChangeLogic.ReviewError, UserNicknameChangeRequest]
  )(implicit request: Request[AnyContent]): Result = {
    if (!isAdmin) {
      AccessDenied
    } else {
      val reason = field("rejectReason").map(_.take(rejectReasonMaxLength))
      SessionLogic.getUser(request) match {
        case None => JsonError(Unauthorized, "Login required.")
        case Some(admin) =>
          database.withTransaction { implicit connection =>
            review(seq, admin.seq, reason)(connection) match {
              case Left(UserNicknameChangeLogic.ReviewError.NotFound) => JsonError(NotFound, "Request not found.")
              case Left(error) => JsonError(BadRequest, error.message)
              case Right(row) => Ok(requestJson(row, User.selectBySeq(row.user)))
            }
          }
      }
    }
  }
}
