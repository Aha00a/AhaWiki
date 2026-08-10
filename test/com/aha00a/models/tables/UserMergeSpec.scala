package com.aha00a.models.tables

import com.aha00a.tests.TestSchema
import anorm.SQL
import anorm.SqlStringInterpolation
import models.tables.UserMerge
import org.scalatest.freespec.AnyFreeSpec

import java.sql.Connection
import java.sql.DriverManager
import java.util.UUID

class UserMergeSpec extends AnyFreeSpec {
  "mergeInto" - {
    "moves user references and deletes the duplicate user" in {
      Class.forName("org.h2.Driver")
      val databaseName = s"user_merge_${UUID.randomUUID().toString.replace("-", "")}"
      val connection = DriverManager.getConnection(s"jdbc:h2:mem:$databaseName;MODE=MySQL;DATABASE_TO_UPPER=false;DB_CLOSE_DELAY=-1")

      try {
        setupSchema(connection)
        insertFixture(connection)

        implicit val implicitConnection: Connection = connection
        UserMerge.mergeInto(canonicalUser = 1L, duplicateUser = 2L)

        assert(count("`User`", "`seq` = 1") === 1L)
        assert(count("`User`", "`seq` = 2") === 0L)
        assert(count("Page", "`user` = 2") === 0L)
        assert(count("AccessLog", "`user` = 2") === 0L)
        assert(count("UserEmail", "`user` = 2") === 0L)
        assert(count("UserEmail", "`user` = 1") === 2L)
        assert(count("UserEmail", "`user` = 1 AND isPrimary = true") === 1L)
      } finally {
        connection.close()
      }
    }
  }

  private def setupSchema(connection: Connection): Unit = {
    TestSchema.createAll()(connection)
  }

  private def insertFixture(connection: Connection): Unit = {
    Seq(
      "INSERT INTO `User` (seq, nickname) VALUES (1, 'canonical')",
      "INSERT INTO `User` (seq, nickname) VALUES (2, 'duplicate')",
      "INSERT INTO UserEmail (`user`, email, isPrimary) VALUES (1, 'canonical@example.com', true)",
      "INSERT INTO UserEmail (`user`, email, isPrimary) VALUES (2, 'duplicate@example.com', true)",
      "INSERT INTO Site (seq, name, abbr) VALUES (1, 'SiteA', 'SiteA')",
      "INSERT INTO Page (site, name, revision, dateTime, `user`, comment) VALUES (1, 'Foo', 1, NOW(), 2, '')",
      "INSERT INTO AccessLog (site, `user`, method, scheme, host, uri, remoteAddress, userAgent, status, durationMilli) VALUES (1, 1, 'GET', 'https', 'localhost', '/', '127.0.0.1', '', 200, 1)",
      "INSERT INTO AccessLog (site, `user`, method, scheme, host, uri, remoteAddress, userAgent, status, durationMilli) VALUES (1, 2, 'GET', 'https', 'localhost', '/', '127.0.0.1', '', 200, 1)",
    ).foreach(sql => SQL(sql).execute()(connection))
  }

  private def count(table: String, where: String)(implicit connection: Connection): Long =
    SQL(s"SELECT COUNT(*) AS cnt FROM $table WHERE $where").as(anorm.SqlParser.long("cnt").single)
}
