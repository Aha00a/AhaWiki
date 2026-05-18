package models.tables

import anorm.SqlParser.{flatten, long, str}
import anorm._
import com.aha00a.play.AnormSqlParser.localDateTime

import java.sql.Connection
import java.time.LocalDateTime

case class PageMeta(
  name: String,
  revision: Long,
  dateInserted: LocalDateTime,
  dateUpdated: Option[LocalDateTime],
  datePageLastChanged: Option[LocalDateTime],
  image: Option[String],
  permRead: String,
  size: Long,
)

object PageMeta {
  private val rowParser = str("name") ~ long("revision") ~ localDateTime("dateInserted") ~ localDateTime("dateUpdated").? ~ localDateTime("datePageLastChanged").? ~ str("image").? ~ str("permRead") ~ long("size")

  def upsert(
    pageName: String,
    revision: Long,
    datePageLastChanged: LocalDateTime,
    image: Option[String],
    permRead: String,
    size: Long,
    dateUpdated: LocalDateTime = LocalDateTime.now(),
  )(implicit connection: Connection, site: Site): Int = {
    SQL"""
      INSERT INTO PageMeta (site, name, revision, dateUpdated, datePageLastChanged, image, permRead, size)
      VALUES (${site.seq}, $pageName, $revision, $dateUpdated, $datePageLastChanged, $image, $permRead, $size)
      ON DUPLICATE KEY UPDATE
        revision = VALUES(revision),
        dateUpdated = VALUES(dateUpdated),
        datePageLastChanged = VALUES(datePageLastChanged),
        image = VALUES(image),
        permRead = VALUES(permRead),
        size = VALUES(size)
    """.executeUpdate()
  }

  def delete(name: String)(implicit connection: Connection, site: Site): Int = {
    SQL"DELETE FROM PageMeta WHERE site = ${site.seq} AND name = $name".executeUpdate()
  }

  //noinspection TypeAnnotation
  def tupled = (apply _).tupled

  def select(name: String)(implicit connection: Connection, site: Site): Option[PageMeta] = {
    SQL"""
      SELECT name, revision, dateInserted, dateUpdated, datePageLastChanged, image, IFNULL(permRead, '') permRead, size
      FROM PageMeta
      WHERE site = ${site.seq} AND name = $name
    """.as(rowParser.singleOpt).map(flatten).map(PageMeta.tupled)
  }
}
