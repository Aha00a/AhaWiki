package models.tables

import anorm.SqlParser._
import anorm._
import com.aha00a.play.AnormSqlParser.localDateTime

import java.sql.Connection
import java.time.LocalDateTime

/**
 * What a nickname used to be, and who changed it.
 *
 * The table arrived with evolution 38 and then waited: nothing read or wrote it until the
 * change-request flow existed to produce rows. Only an approval writes here — asking for a
 * nickname is not a change, and a rejected or withdrawn request never became one.
 */
case class UserNicknameHistory(
  seq: Long,
  user: Long,
  old: String,
  updated: String,
  changedBy: Long,
  dateInserted: LocalDateTime,
)

object UserNicknameHistory {
  //noinspection TypeAnnotation
  def tupled = (apply _).tupled

  private val rowParser =
    long("seq") ~ long("user") ~ str("old") ~ str("new") ~ long("changedBy") ~ localDateTime("dateInserted")

  def insert(user: Long, old: String, updated: String, changedBy: Long)(implicit connection: Connection): Option[Long] =
    SQL"""
      INSERT INTO UserNicknameHistory (`user`, `old`, `new`, changedBy)
      VALUES ($user, $old, $updated, $changedBy)
    """.executeInsert()

  def selectByUser(user: Long)(implicit connection: Connection): Seq[UserNicknameHistory] =
    SQL"""
      SELECT seq, `user`, `old`, `new`, changedBy, dateInserted
      FROM UserNicknameHistory
      WHERE `user` = $user
      ORDER BY dateInserted DESC, seq DESC
    """.as(rowParser.*).map(flatten).map(UserNicknameHistory.tupled)
}
