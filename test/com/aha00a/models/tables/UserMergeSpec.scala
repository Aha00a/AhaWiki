package com.aha00a.models.tables

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
        assert(count("UserSite", "`user` = 2") === 0L)
        assert(count("UserSite", "`user` = 1 AND site = 1") === 1L)
        assert(count("UserSite", "`user` = 1 AND site = 2") === 1L)
      } finally {
        connection.close()
      }
    }
  }

  private def setupSchema(connection: Connection): Unit = {
    Seq(
      """
        CREATE TABLE `User` (
          seq BIGINT AUTO_INCREMENT PRIMARY KEY,
          created DATETIME DEFAULT CURRENT_TIMESTAMP NOT NULL,
          updated DATETIME DEFAULT CURRENT_TIMESTAMP NOT NULL,
          nickname VARCHAR(255) NOT NULL,
          profileImageUrl VARCHAR(512) NULL
        )
      """,
      """
        CREATE TABLE UserEmail (
          `user` BIGINT NOT NULL,
          email VARCHAR(255) NOT NULL,
          isPrimary BOOLEAN NOT NULL DEFAULT FALSE,
          created DATETIME DEFAULT CURRENT_TIMESTAMP NOT NULL,
          PRIMARY KEY (`user`, email),
          UNIQUE (email),
          FOREIGN KEY (`user`) REFERENCES `User` (seq)
        )
      """,
      """
        CREATE TABLE Page (
          seq BIGINT AUTO_INCREMENT PRIMARY KEY,
          `user` BIGINT NOT NULL,
          FOREIGN KEY (`user`) REFERENCES `User` (seq)
        )
      """,
      """
        CREATE TABLE AccessLog (
          seq BIGINT AUTO_INCREMENT PRIMARY KEY,
          `user` BIGINT NULL,
          FOREIGN KEY (`user`) REFERENCES `User` (seq)
        )
      """,
      """
        CREATE TABLE UserSite (
          `user` BIGINT NOT NULL,
          site BIGINT NOT NULL,
          created DATETIME DEFAULT CURRENT_TIMESTAMP NOT NULL,
          PRIMARY KEY (`user`, site),
          FOREIGN KEY (`user`) REFERENCES `User` (seq)
        )
      """,
    ).foreach(sql => SQL(sql).execute()(connection))
  }

  private def insertFixture(connection: Connection): Unit = {
    Seq(
      "INSERT INTO `User` (seq, nickname) VALUES (1, 'canonical')",
      "INSERT INTO `User` (seq, nickname) VALUES (2, 'duplicate')",
      "INSERT INTO UserEmail (`user`, email, isPrimary) VALUES (1, 'canonical@example.com', true)",
      "INSERT INTO UserEmail (`user`, email, isPrimary) VALUES (2, 'duplicate@example.com', true)",
      "INSERT INTO Page (`user`) VALUES (2)",
      "INSERT INTO AccessLog (`user`) VALUES (1)",
      "INSERT INTO AccessLog (`user`) VALUES (2)",
      "INSERT INTO UserSite (`user`, site) VALUES (1, 1)",
      "INSERT INTO UserSite (`user`, site) VALUES (2, 1)",
      "INSERT INTO UserSite (`user`, site) VALUES (2, 2)",
    ).foreach(sql => SQL(sql).execute()(connection))
  }

  private def count(table: String, where: String)(implicit connection: Connection): Long =
    SQL(s"SELECT COUNT(*) AS cnt FROM $table WHERE $where").as(anorm.SqlParser.long("cnt").single)
}
