package models.tables

import anorm.SqlParser._
import anorm._
import com.aha00a.play.AnormSqlParser.localDateTime

import java.sql.Connection
import java.time.LocalDateTime

case class SiteAdmin(site: Long, user: Long, dateInserted: LocalDateTime)

object SiteAdmin {
  //noinspection TypeAnnotation
  def tupled = (apply _).tupled

  private val rowParser = long("site") ~ long("user") ~ localDateTime("dateInserted")

  def exists(site: Long, user: Long)(implicit connection: Connection): Boolean =
    SQL"""
      SELECT COUNT(*) AS cnt
      FROM SiteAdmin
      WHERE site = $site
        AND `user` = $user
    """.as(long("cnt").single) > 0

  def selectByUser(user: Long)(implicit connection: Connection): Seq[SiteAdmin] =
    SQL"""
      SELECT site, `user`, dateInserted
      FROM SiteAdmin
      WHERE `user` = $user
      ORDER BY site
    """.as(rowParser.*).map(flatten).map(SiteAdmin.tupled)

  def selectBySite(site: Long)(implicit connection: Connection): Seq[SiteAdmin] =
    SQL"""
      SELECT site, `user`, dateInserted
      FROM SiteAdmin
      WHERE site = $site
      ORDER BY dateInserted
    """.as(rowParser.*).map(flatten).map(SiteAdmin.tupled)

  def insert(site: Long, user: Long)(implicit connection: Connection): Int =
    SQL"""
      INSERT INTO SiteAdmin (site, `user`)
      VALUES ($site, $user)
    """.executeUpdate()

  def delete(site: Long, user: Long)(implicit connection: Connection): Int =
    SQL"""
      DELETE FROM SiteAdmin
      WHERE site = $site
        AND `user` = $user
    """.executeUpdate()
}
