package models.tables

import anorm.SqlParser.bool
import anorm.SqlParser.long
import anorm.SqlParser.str
import anorm._
import com.aha00a.play.AnormSqlParser._

import java.sql.Connection
import java.time.LocalDateTime

/**
 * A deleted page, kept so a site admin can restore it (evolution 75, PageTrashLogic).
 *
 * The revisions are copied with INSERT ... SELECT rather than read into the JVM and written back:
 * a page's history can be large, and nothing here needs to look at it.
 */
case class DeletedPage(
  seq: Long,
  name: String,
  dateDeleted: LocalDateTime,
  deletedBy: Option[Long],
  deletedByNickname: Option[String],
  viaApi: Boolean,
  lastRevision: Long,
  revisionCount: Long,
  attachmentCount: Long,
)

object DeletedPage {
  private val parser: RowParser[DeletedPage] =
    (long("seq") ~ str("name") ~ localDateTime("dateDeleted") ~ optionLong("deletedBy") ~ optionStr("nickname") ~
      bool("viaApi") ~ long("lastRevision") ~ long("revisionCount") ~ long("attachmentCount")).map {
      case seq ~ name ~ dateDeleted ~ deletedBy ~ nickname ~ viaApi ~ lastRevision ~ revisionCount ~ attachmentCount =>
        DeletedPage(seq, name, dateDeleted, deletedBy, nickname, viaApi, lastRevision, revisionCount, attachmentCount)
    }

  /**
   * Copies every revision of the page into the kept tables and returns the new row's seq, or None
   * when the page has no revision. Page itself is left alone: the caller deletes it in the same
   * transaction.
   */
  def insertFromPage(name: String, deletedBy: Option[Long], viaApi: Boolean, now: LocalDateTime)
                    (implicit connection: Connection, site: Site): Option[Long] = {
    val (lastRevision, revisionCount) = SQL"""
      SELECT COALESCE(MAX(revision), 0) AS lastRevision, COUNT(*) AS revisionCount
      FROM Page WHERE site = ${site.seq} AND name = $name
    """.as((long("lastRevision") ~ long("revisionCount")).map { case a ~ b => (a, b) }.single)
    if (revisionCount == 0) {
      None
    } else {
      val seq: Long = SQL"""
        INSERT INTO DeletedPage (site, name, dateDeleted, deletedBy, viaApi, lastRevision, revisionCount)
        VALUES (${site.seq}, $name, $now, ${deletedBy.map(_.toString).getOrElse(null)}, $viaApi, $lastRevision, $revisionCount)
      """.executeInsert(SqlParser.scalar[Long].single)
      SQL"""
        INSERT INTO DeletedPageRevision
            (deletedPage, revision, dateTime, `user`, remoteAddress, comment, isMinorEdit, viaApi, userApiKey, content)
        SELECT $seq, revision, dateTime, `user`, remoteAddress, comment, isMinorEdit, viaApi, userApiKey, content
        FROM Page WHERE site = ${site.seq} AND name = $name
      """.executeUpdate()
      Some(seq)
    }
  }

  /** Newest first, with how many attachments each keeps in S3. */
  def selectBySite()(implicit connection: Connection, site: Site): Seq[DeletedPage] =
    SQL"""
      SELECT d.seq, d.name, d.dateDeleted, d.deletedBy, u.nickname, d.viaApi, d.lastRevision, d.revisionCount,
             (SELECT COUNT(*) FROM Attachment a WHERE a.deletedPage = d.seq AND a.status = 'Trashed') AS attachmentCount
      FROM DeletedPage d
      LEFT JOIN `User` u ON u.seq = d.deletedBy
      WHERE d.site = ${site.seq}
      ORDER BY d.dateDeleted DESC, d.seq DESC
    """.as(parser.*)

  def select(seq: Long)(implicit connection: Connection, site: Site): Option[DeletedPage] =
    SQL"""
      SELECT d.seq, d.name, d.dateDeleted, d.deletedBy, u.nickname, d.viaApi, d.lastRevision, d.revisionCount,
             (SELECT COUNT(*) FROM Attachment a WHERE a.deletedPage = d.seq AND a.status = 'Trashed') AS attachmentCount
      FROM DeletedPage d
      LEFT JOIN `User` u ON u.seq = d.deletedBy
      WHERE d.site = ${site.seq} AND d.seq = $seq
    """.as(parser.singleOpt)

  /** The text of the last kept revision, for an admin deciding whether to restore. */
  def selectLastContent(seq: Long)(implicit connection: Connection, site: Site): Option[String] =
    SQL"""
      SELECT r.content
      FROM DeletedPageRevision r
      JOIN DeletedPage d ON d.seq = r.deletedPage
      WHERE d.site = ${site.seq} AND d.seq = $seq AND r.revision = d.lastRevision
    """.as(SqlParser.get[Option[String]]("content").singleOpt).map(_.getOrElse(""))

  /** Writes the kept revisions back to Page under the kept name. The caller has checked the name is free. */
  def copyRevisionsToPage(seq: Long)(implicit connection: Connection, site: Site): Int =
    SQL"""
      INSERT INTO Page (site, name, revision, dateTime, `user`, remoteAddress, comment, isMinorEdit, viaApi, userApiKey, content)
      SELECT d.site, d.name, r.revision, r.dateTime, r.`user`, r.remoteAddress, r.comment, r.isMinorEdit, r.viaApi, r.userApiKey, r.content
      FROM DeletedPageRevision r
      JOIN DeletedPage d ON d.seq = r.deletedPage
      WHERE d.site = ${site.seq} AND d.seq = $seq
    """.executeUpdate()

  /** Removes the kept page; its revisions go with it (ON DELETE CASCADE). */
  def delete(seq: Long)(implicit connection: Connection, site: Site): Int =
    SQL"DELETE FROM DeletedPage WHERE site = ${site.seq} AND seq = $seq".executeUpdate()
}
