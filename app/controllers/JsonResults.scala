package controllers

import io.circe.Json
import play.api.mvc._

/**
 * Response helpers shared by the controllers that speak JSON.
 *
 * There is exactly one error envelope, `{"error": "<message>"}`, and it is built here.
 * Rewriting it per controller is how the notation drifts apart — two idioms for the same
 * envelope, `Json.obj` and `Map(...).asJson`, were already in use side by side.
 */
trait JsonResults extends BaseController {
  def Ok(json: Json): Result = Ok(json.toString()).as(JSON)

  def JsonResult(status: Status, json: Json): Result = status(json.toString()).as(JSON)

  def JsonError(status: Status, message: String): Result =
    JsonResult(status, Json.obj("error" -> Json.fromString(message)))

  /**
   * The envelope every paged admin list answers with.
   *
   * Four endpoints built this by hand and had already drifted: three sent `page` and
   * `pageSize`, one sent only `array` and `count`. The admin UI tolerated both, so nothing
   * pointed the difference out — a client that started paging from the fourth endpoint would
   * have found the fields it needed missing.
   */
  def pagedJson(array: Json, page: Int, pageSize: Int, count: Long): Json = Json.obj(
    "array" -> array,
    "page" -> Json.fromInt(page),
    "pageSize" -> Json.fromInt(pageSize),
    "count" -> Json.fromLong(count),
  )
}
