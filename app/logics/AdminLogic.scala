package logics

import play.api.mvc.RequestHeader

object AdminLogic {
  def isAdmin(request: RequestHeader): Boolean =
    SessionLogic.getUser(request).exists(_.seq == 1)
}
