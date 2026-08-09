package controllers

import io.circe.Json
import logics.SessionLogic
import models.tables.User
import models.tables.UserApiKey
import play.api.db.Database
import play.api.mvc._

import javax.inject._

/**
 * API keys, from both ends: a user managing their own, and an admin revoking anyone's.
 *
 * The two live together because they answer with the same object. The raw key appears in
 * exactly one response — the one that creates it — and is never readable again, so the
 * shared shape deliberately does not carry it.
 */
class ApiApiKey @Inject()(
  implicit val
  controllerComponents: ControllerComponents,
  database: Database,
) extends BaseController with JsonResults with AdminAuth {

  private val nameMaxLength = 255

  private def apiKeyJson(apiKey: UserApiKey, user: Option[User] = None): Json = Json.obj(
    "seq" -> Json.fromLong(apiKey.seq),
    "user" -> Json.fromLong(apiKey.user),
    "userNickname" -> user.map(u => Json.fromString(u.nickname)).getOrElse(Json.Null),
    "keyPrefix" -> Json.fromString(apiKey.keyPrefix),
    "name" -> Json.fromString(apiKey.name),
    "dateInserted" -> Json.fromString(apiKey.dateInserted.toString),
    "dateLastUsed" -> apiKey.dateLastUsed.map(v => Json.fromString(v.toString)).getOrElse(Json.Null),
    "dateRevoked" -> apiKey.dateRevoked.map(v => Json.fromString(v.toString)).getOrElse(Json.Null),
    "revoked" -> Json.fromBoolean(apiKey.dateRevoked.nonEmpty),
  )

  /** The admin UI posts JSON and the account page posts a form, so both are accepted. */
  private def apiKeyNameFromRequest(request: Request[AnyContent]): String = {
    request.body.asJson
      .flatMap(json => (json \ "name").asOpt[String])
      .orElse(request.body.asFormUrlEncoded.flatMap(_.get("name").flatMap(_.headOption)))
      .map(_.trim)
      .getOrElse("")
  }

  private def withLoginUser(block: User.SessionUser => Result)(implicit request: RequestHeader): Result =
    SessionLogic.getUser(request).fold(JsonError(Unauthorized, "Login required."))(block)

  def accountApiKeys: Action[AnyContent] = Action { implicit request =>
    withLoginUser { user =>
      database.withConnection { implicit connection =>
        Ok(Json.fromValues(UserApiKey.selectByUser(user.seq).map(apiKey => apiKeyJson(apiKey))))
      }
    }
  }

  def accountCreateApiKey: Action[AnyContent] = Action { implicit request =>
    withLoginUser { user =>
      val name = apiKeyNameFromRequest(request)
      if (name.isEmpty) {
        JsonError(BadRequest, "name is required.")
      } else if (name.length > nameMaxLength) {
        JsonError(BadRequest, "name is too long.")
      } else {
        database.withConnection { implicit connection =>
          val created = UserApiKey.insert(user.seq, name)
          Ok(apiKeyJson(created.row).deepMerge(Json.obj("key" -> Json.fromString(created.rawKey))))
        }
      }
    }
  }

  def accountRevokeApiKey(seq: Long): Action[AnyContent] = Action { implicit request =>
    withLoginUser { user =>
      database.withConnection { implicit connection =>
        // Scoped to the caller: revoking by seq alone would let anyone revoke anyone's key.
        val updated = UserApiKey.revokeByUser(seq, user.seq)
        if (updated == 0) JsonError(NotFound, "API key not found.")
        else Ok(Json.obj("ok" -> Json.fromBoolean(true), "seq" -> Json.fromLong(seq)))
      }
    }
  }

  def adminApiKeys: Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      AccessDenied
    } else {
      database.withConnection { implicit connection =>
        val rows = UserApiKey.selectAll().map { apiKey =>
          apiKeyJson(apiKey, User.selectBySeq(apiKey.user))
        }
        Ok(Json.fromValues(rows))
      }
    }
  }

  def adminRevokeApiKey(seq: Long): Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      AccessDenied
    } else {
      database.withConnection { implicit connection =>
        val updated = UserApiKey.revoke(seq)
        if (updated == 0) JsonError(NotFound, "API key not found.")
        else Ok(Json.obj("ok" -> Json.fromBoolean(true), "seq" -> Json.fromLong(seq)))
      }
    }
  }
}
