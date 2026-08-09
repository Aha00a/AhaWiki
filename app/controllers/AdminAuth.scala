package controllers

import logics.AdminLogic
import play.api.db.Database
import play.api.mvc._

/**
 * The admin permission check and the response returned when it fails.
 *
 * Both live here because they change together.
 *
 * `Database` arrives as an implicit parameter rather than an abstract member: the
 * controllers declare `database` as an implicit constructor parameter, not a `val`, so it
 * cannot implement one. Making it able to would mean adding `val` to every controller and
 * widening their public surface for nothing.
 *
 * The rejection is `text/plain`. Changing that envelope without first checking what the
 * clients parse would break them silently, so it is left to a separate change.
 */
trait AdminAuth { self: BaseController =>
  protected def isAdmin(implicit request: RequestHeader): Boolean =
    AdminLogic.isAdmin(request)

  protected def isSiteAdmin(siteSeq: Long)(implicit request: RequestHeader, database: Database): Boolean =
    AdminLogic.isSiteAdmin(siteSeq, request)(database)

  // A def, not a val: a val in a trait that loses an initialization-order race still
  // compiles and only surfaces as an NPE at construction. Building a Result costs nothing
  // worth that risk.
  protected def AccessDenied: Result = Forbidden("Access denied.")
}
