package models.tables

import anorm.SqlParser._
import anorm._
import com.aha00a.play.AnormSqlParser.localDateTime

import java.sql.Connection
import java.time.LocalDateTime

case class SiteRequest(
  seq: Long,
  user: Long,
  name: String,
  subdomain: String,
  isPublic: Boolean,
  purpose: String,
  status: String,
  reviewedBy: Option[Long],
  rejectReason: Option[String],
  site: Option[Long],
  dateInserted: LocalDateTime,
  dateReviewed: Option[LocalDateTime],
) {
  def isPending: Boolean = status == SiteRequest.Status.Pending.toString
}

object SiteRequest {

  // Same shape as UserNicknameChangeRequest and kept apart from it: the two answer different
  // questions, and an approval here creates a site, so they have no reason to change together.
  object Status extends Enumeration {
    val Pending, Approved, Rejected, Canceled = Value
  }

  //noinspection TypeAnnotation
  def tupled = (apply _).tupled

  private val rowParser =
    long("seq") ~ long("user") ~ str("name") ~ str("subdomain") ~ bool("isPublic") ~ str("purpose") ~ str("status") ~
      long("reviewedBy").? ~ str("rejectReason").? ~ long("site").? ~ localDateTime("dateInserted") ~ localDateTime("dateReviewed").?

  private val columns = "seq, `user`, name, subdomain, isPublic, purpose, status, reviewedBy, rejectReason, site, dateInserted, dateReviewed"

  def insert(user: Long, name: String, subdomain: String, isPublic: Boolean, purpose: String)(implicit connection: Connection): Option[Long] =
    SQL"""
      INSERT INTO SiteRequest (`user`, name, subdomain, isPublic, purpose, status)
      VALUES ($user, $name, $subdomain, $isPublic, $purpose, ${Status.Pending.toString})
    """.executeInsert()

  def selectBySeq(seq: Long)(implicit connection: Connection): Option[SiteRequest] =
    SQL(s"SELECT $columns FROM SiteRequest WHERE seq = {seq}")
      .on("seq" -> seq)
      .as(rowParser.singleOpt).map(flatten).map(SiteRequest.tupled)

  /** Held for the transaction, so that two admins approving at once cannot make two sites. */
  def selectBySeqForUpdate(seq: Long)(implicit connection: Connection): Option[SiteRequest] =
    SQL(s"SELECT $columns FROM SiteRequest WHERE seq = {seq} FOR UPDATE")
      .on("seq" -> seq)
      .as(rowParser.singleOpt).map(flatten).map(SiteRequest.tupled)

  def selectPendingByUser(user: Long)(implicit connection: Connection): Option[SiteRequest] =
    SQL(s"SELECT $columns FROM SiteRequest WHERE `user` = {user} AND status = {status} ORDER BY seq DESC")
      .on("user" -> user, "status" -> Status.Pending.toString)
      .as(rowParser.*).map(flatten).map(SiteRequest.tupled).headOption

  def selectPendingBySubdomain(subdomain: String)(implicit connection: Connection): Option[SiteRequest] =
    SQL(s"SELECT $columns FROM SiteRequest WHERE subdomain = {subdomain} AND status = {status} ORDER BY seq DESC")
      .on("subdomain" -> subdomain, "status" -> Status.Pending.toString)
      .as(rowParser.*).map(flatten).map(SiteRequest.tupled).headOption

  def selectByUser(user: Long, limit: Int)(implicit connection: Connection): Seq[SiteRequest] =
    SQL(s"SELECT $columns FROM SiteRequest WHERE `user` = {user} ORDER BY seq DESC LIMIT {limit}")
      .on("user" -> user, "limit" -> limit)
      .as(rowParser.*).map(flatten).map(SiteRequest.tupled)

  def selectByStatus(status: Status.Value)(implicit connection: Connection): Seq[SiteRequest] =
    SQL(s"SELECT $columns FROM SiteRequest WHERE status = {status} ORDER BY dateInserted, seq")
      .on("status" -> status.toString)
      .as(rowParser.*).map(flatten).map(SiteRequest.tupled)

  def selectAll(limit: Int)(implicit connection: Connection): Seq[SiteRequest] =
    SQL(s"SELECT $columns FROM SiteRequest ORDER BY seq DESC LIMIT {limit}")
      .on("limit" -> limit)
      .as(rowParser.*).map(flatten).map(SiteRequest.tupled)

  /** The status is in the WHERE, so the update itself decides and a second answer returns 0. */
  def review(seq: Long, status: Status.Value, reviewedBy: Long, rejectReason: Option[String], site: Option[Long])(implicit connection: Connection): Int =
    SQL"""
      UPDATE SiteRequest
      SET status = ${status.toString}, reviewedBy = $reviewedBy, rejectReason = $rejectReason, site = $site, dateReviewed = ${LocalDateTime.now()}
      WHERE seq = $seq AND status = ${Status.Pending.toString}
    """.executeUpdate()

  /** Withdrawal by the requester, which is why it is scoped to them rather than taken by seq. */
  def cancelPendingByUser(seq: Long, user: Long)(implicit connection: Connection): Int =
    SQL"""
      UPDATE SiteRequest
      SET status = ${Status.Canceled.toString}, dateReviewed = ${LocalDateTime.now()}
      WHERE seq = $seq AND `user` = $user AND status = ${Status.Pending.toString}
    """.executeUpdate()
}
