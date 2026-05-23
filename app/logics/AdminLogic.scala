package logics

import play.api.db.Database
import play.api.mvc.RequestHeader

object AdminLogic {
  def isAdmin(request: RequestHeader): Boolean =
    SessionLogic.getUser(request).exists(_.seq == 1)

  def isAdmin(requestWrapper: models.RequestWrapper): Boolean =
    requestWrapper.getUser.exists(_.seq == 1)

  def isSiteAdminBySeq(siteSeq: Long, userSeq: Long)(implicit db: Database): Boolean =
    userSeq == 1 || db.withConnection(implicit c => AhaWikiCacheMemorySiteAdmin.getUserSeqs(siteSeq).contains(userSeq))

  def isSiteAdmin(siteSeq: Long, request: RequestHeader)(implicit db: Database): Boolean =
    isAdmin(request) ||
      SessionLogic.getUser(request).exists(u => isSiteAdminBySeq(siteSeq, u.seq))

  def isSiteAdmin(siteSeq: Long, requestWrapper: models.RequestWrapper)(implicit db: Database): Boolean =
    isAdmin(requestWrapper) ||
      requestWrapper.getUser.exists(u => isSiteAdminBySeq(siteSeq, u.seq))
}
