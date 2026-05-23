package logics

import models.tables.SiteAdmin
import play.api.db.Database
import play.api.mvc.RequestHeader

object AdminLogic {
  def isAdmin(request: RequestHeader): Boolean =
    SessionLogic.getUser(request).exists(_.seq == 1)

  def isSiteAdmin(siteSeq: Long, request: RequestHeader)(implicit db: Database): Boolean =
    isAdmin(request) ||
      SessionLogic.getUser(request).exists(u => db.withConnection(implicit c => SiteAdmin.exists(siteSeq, u.seq)))
}
