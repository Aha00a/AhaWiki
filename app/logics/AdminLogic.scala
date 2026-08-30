package logics

import play.api.db.Database
import play.api.mvc.RequestHeader

object AdminLogic {

  /**
   * The instance-wide administrator, identified by being the first row of `User`.
   *
   * There is no role table behind this, deliberately — the AccessControl page records the
   * decision, what it costs, and what to build instead once a second instance-wide
   * administrator is actually needed. The number lives here alone because it used to stand
   * unnamed in three places, with nothing saying what it meant.
   */
  val SuperAdminUserSeq: Long = 1L

  def isAdmin(request: RequestHeader): Boolean =
    SessionLogic.getUser(request).exists(_.seq == SuperAdminUserSeq)

  def isAdmin(requestWrapper: models.RequestWrapper): Boolean =
    requestWrapper.getUser.exists(_.seq == SuperAdminUserSeq)

  def isSiteAdminBySeq(siteSeq: Long, userSeq: Long)(implicit db: Database): Boolean =
    userSeq == SuperAdminUserSeq || db.withConnection(implicit c => AhaWikiCacheMemorySiteAdmin.getUserSeqs(siteSeq).contains(userSeq))

  def isSiteAdmin(siteSeq: Long, request: RequestHeader)(implicit db: Database): Boolean =
    isAdmin(request) ||
      SessionLogic.getUser(request).exists(u => isSiteAdminBySeq(siteSeq, u.seq))

  def isSiteAdmin(siteSeq: Long, requestWrapper: models.RequestWrapper)(implicit db: Database): Boolean =
    isAdmin(requestWrapper) ||
      requestWrapper.getUser.exists(u => isSiteAdminBySeq(siteSeq, u.seq))
}
