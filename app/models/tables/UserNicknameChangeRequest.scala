package models.tables

import anorm.SqlParser._
import anorm._
import com.aha00a.play.AnormSqlParser.localDateTime

import java.sql.Connection
import java.time.LocalDateTime

case class UserNicknameChangeRequest(
  seq: Long,
  user: Long,
  requestedNickname: String,
  status: String,
  requestedBy: Long,
  reviewedBy: Option[Long],
  rejectReason: Option[String],
  dateInserted: LocalDateTime,
  dateReviewed: Option[LocalDateTime],
) {
  def isPending: Boolean = status == UserNicknameChangeRequest.Status.Pending.toString
}

object UserNicknameChangeRequest {

  /**
   * `Canceled` is not in the original plan, which named only Pending, Approved and Rejected.
   * The plan also says a user may withdraw their own pending request, and deleting the row
   * would be the only other way to do that — which loses the fact that they asked at all,
   * and with it the answer to "why does this user keep appearing in the log". A fourth state
   * costs nothing: the column is a VARCHAR and the queries filter by name.
   */
  object Status extends Enumeration {
    val Pending, Approved, Rejected, Canceled = Value
  }

  //noinspection TypeAnnotation
  def tupled = (apply _).tupled

  private val rowParser =
    long("seq") ~ long("user") ~ str("requestedNickname") ~ str("status") ~ long("requestedBy") ~
      long("reviewedBy").? ~ str("rejectReason").? ~ localDateTime("dateInserted") ~ localDateTime("dateReviewed").?

  private val columns = "seq, `user`, requestedNickname, status, requestedBy, reviewedBy, rejectReason, dateInserted, dateReviewed"

  def insert(user: Long, requestedNickname: String, requestedBy: Long)(implicit connection: Connection): Option[Long] =
    SQL"""
      INSERT INTO UserNicknameChangeRequest (`user`, requestedNickname, status, requestedBy)
      VALUES ($user, $requestedNickname, ${Status.Pending.toString}, $requestedBy)
    """.executeInsert()

  def selectBySeq(seq: Long)(implicit connection: Connection): Option[UserNicknameChangeRequest] =
    SQL(s"SELECT $columns FROM UserNicknameChangeRequest WHERE seq = {seq}")
      .on("seq" -> seq)
      .as(rowParser.singleOpt).map(flatten).map(UserNicknameChangeRequest.tupled)

  /**
   * The same row, held for the rest of the transaction.
   *
   * Approval reads the request, checks it is still pending, and only then writes. Without the
   * lock two admins clicking Approve at the same moment both read Pending, and the second one
   * rewrites a nickname that was already changed — and writes a second history row saying it
   * changed from a name it never had.
   */
  def selectBySeqForUpdate(seq: Long)(implicit connection: Connection): Option[UserNicknameChangeRequest] =
    SQL(s"SELECT $columns FROM UserNicknameChangeRequest WHERE seq = {seq} FOR UPDATE")
      .on("seq" -> seq)
      .as(rowParser.singleOpt).map(flatten).map(UserNicknameChangeRequest.tupled)

  def selectPendingByUser(user: Long)(implicit connection: Connection): Option[UserNicknameChangeRequest] =
    SQL(s"SELECT $columns FROM UserNicknameChangeRequest WHERE `user` = {user} AND status = {status} ORDER BY seq DESC")
      .on("user" -> user, "status" -> Status.Pending.toString)
      .as(rowParser.*).map(flatten).map(UserNicknameChangeRequest.tupled).headOption

  /** Case-insensitive, because `requestedNickname` carries `User.nickname`'s collation. */
  def selectPendingByNickname(nickname: String)(implicit connection: Connection): Option[UserNicknameChangeRequest] =
    SQL(s"SELECT $columns FROM UserNicknameChangeRequest WHERE requestedNickname = {nickname} AND status = {status} ORDER BY seq DESC")
      .on("nickname" -> nickname, "status" -> Status.Pending.toString)
      .as(rowParser.*).map(flatten).map(UserNicknameChangeRequest.tupled).headOption

  def selectByUser(user: Long, limit: Int)(implicit connection: Connection): Seq[UserNicknameChangeRequest] =
    SQL(s"SELECT $columns FROM UserNicknameChangeRequest WHERE `user` = {user} ORDER BY seq DESC LIMIT {limit}")
      .on("user" -> user, "limit" -> limit)
      .as(rowParser.*).map(flatten).map(UserNicknameChangeRequest.tupled)

  def selectByStatus(status: Status.Value)(implicit connection: Connection): Seq[UserNicknameChangeRequest] =
    SQL(s"SELECT $columns FROM UserNicknameChangeRequest WHERE status = {status} ORDER BY dateInserted, seq")
      .on("status" -> status.toString)
      .as(rowParser.*).map(flatten).map(UserNicknameChangeRequest.tupled)

  def selectAll(limit: Int)(implicit connection: Connection): Seq[UserNicknameChangeRequest] =
    SQL(s"SELECT $columns FROM UserNicknameChangeRequest ORDER BY seq DESC LIMIT {limit}")
      .on("limit" -> limit)
      .as(rowParser.*).map(flatten).map(UserNicknameChangeRequest.tupled)

  /**
   * Moves a request out of Pending, and only out of Pending.
   *
   * The status is part of the WHERE rather than checked beforehand, so the update itself is
   * the thing that decides. A caller that reads Pending and then updates has a window; this
   * returns 0 instead.
   */
  def review(seq: Long, status: Status.Value, reviewedBy: Long, rejectReason: Option[String])(implicit connection: Connection): Int =
    SQL"""
      UPDATE UserNicknameChangeRequest
      SET status = ${status.toString}, reviewedBy = $reviewedBy, rejectReason = $rejectReason, dateReviewed = ${LocalDateTime.now()}
      WHERE seq = $seq AND status = ${Status.Pending.toString}
    """.executeUpdate()

  /** Withdrawal by the requester, which is why it is scoped to them rather than taken by seq. */
  def cancelPendingByUser(seq: Long, user: Long)(implicit connection: Connection): Int =
    SQL"""
      UPDATE UserNicknameChangeRequest
      SET status = ${Status.Canceled.toString}, dateReviewed = ${LocalDateTime.now()}
      WHERE seq = $seq AND `user` = $user AND status = ${Status.Pending.toString}
    """.executeUpdate()
}
