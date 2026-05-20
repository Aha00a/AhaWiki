package models.tables

import anorm._

import java.sql.Connection

object UserMerge {
  private case class UserReference(table: String, column: String)

  private def hasColumn(table: String, column: String)(implicit connection: Connection): Boolean = {
    SQL"""
      SELECT COUNT(*) AS cnt
      FROM INFORMATION_SCHEMA.COLUMNS
      WHERE TABLE_SCHEMA = DATABASE()
        AND TABLE_NAME = $table
        AND COLUMN_NAME = $column
    """.as(anorm.SqlParser.long("cnt").single) > 0
  }

  private def quoteIdentifier(identifier: String): String =
    "`" + identifier.replace("`", "``") + "`"

  private def selectUserReferences()(implicit connection: Connection): Seq[UserReference] = {
    SQL"""
      SELECT TABLE_NAME, COLUMN_NAME
      FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE
      WHERE TABLE_SCHEMA = DATABASE()
        AND REFERENCED_TABLE_NAME = 'User'
        AND REFERENCED_COLUMN_NAME = 'seq'
    """.as((anorm.SqlParser.str("TABLE_NAME") ~ anorm.SqlParser.str("COLUMN_NAME")).map {
      case table ~ column => UserReference(table, column)
    }.*)
  }

  private def updateUserReference(table: String, column: String, canonicalUser: Long, duplicateUser: Long)
                                 (implicit connection: Connection): Int = {
    SQL(s"UPDATE ${quoteIdentifier(table)} SET ${quoteIdentifier(column)} = {canonicalUser} WHERE ${quoteIdentifier(column)} = {duplicateUser}")
      .on(
        "canonicalUser" -> canonicalUser,
        "duplicateUser" -> duplicateUser,
      )
      .executeUpdate()
  }

  private def mergeUserSiteIfPresent(canonicalUser: Long, duplicateUser: Long)(implicit connection: Connection): Unit = {
    if (hasColumn("UserSite", "user") && hasColumn("UserSite", "site")) {
      if (hasColumn("UserSite", "created")) {
        SQL"""
          INSERT IGNORE INTO UserSite (user, site, created)
          SELECT $canonicalUser, site, created
          FROM UserSite
          WHERE user = $duplicateUser
        """.executeUpdate()
      } else {
        SQL"""
          INSERT IGNORE INTO UserSite (user, site)
          SELECT $canonicalUser, site
          FROM UserSite
          WHERE user = $duplicateUser
        """.executeUpdate()
      }
      SQL"DELETE FROM UserSite WHERE user = $duplicateUser".executeUpdate()
    }
  }

  private def withLocalTransaction[T](f: => T)(implicit connection: Connection): T = {
    if (connection.getAutoCommit) {
      connection.setAutoCommit(false)
      try {
        val result = f
        connection.commit()
        result
      } catch {
        case e: Throwable =>
          connection.rollback()
          throw e
      } finally {
        connection.setAutoCommit(true)
      }
    } else {
      f
    }
  }

  def mergeInto(canonicalUser: Long, duplicateUser: Long)(implicit connection: Connection): Unit = {
    if (canonicalUser == duplicateUser) return

    withLocalTransaction {
      SQL"UPDATE UserEmail SET isPrimary = false WHERE user = $duplicateUser".executeUpdate()

      mergeUserSiteIfPresent(canonicalUser, duplicateUser)
      selectUserReferences()
        .filterNot(_.table == "UserSite")
        .foreach(reference => updateUserReference(reference.table, reference.column, canonicalUser, duplicateUser))

      if (!UserEmail.hasPrimary(canonicalUser)) {
        UserEmail.selectByUser(canonicalUser).headOption.foreach { userEmail =>
          UserEmail.setPrimary(canonicalUser, userEmail.email)
        }
      }

      SQL"DELETE FROM User WHERE seq = $duplicateUser".executeUpdate()
    }
  }
}
