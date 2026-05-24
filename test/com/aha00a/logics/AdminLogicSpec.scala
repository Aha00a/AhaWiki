package com.aha00a.logics

import anorm.SQL
import logics.AdminLogic
import models.tables.SiteAdmin
import org.scalatest.freespec.AnyFreeSpec
import play.api.db.Database

import java.sql.Connection
import java.sql.DriverManager

class AdminLogicSpec extends AnyFreeSpec {
  private def withDatabase[T](f: Database => T): T = {
    Class.forName("org.h2.Driver")
    val dbName = s"adminlogic_${java.util.UUID.randomUUID().toString.replace("-", "")}"
    val connection = DriverManager.getConnection(s"jdbc:h2:mem:$dbName;MODE=MySQL;DB_CLOSE_DELAY=-1")
    setupSchema(connection)

    val db = new Database {
      override def name: String = dbName
      override def dataSource: javax.sql.DataSource = throw new UnsupportedOperationException
      override def url: String = s"jdbc:h2:mem:$dbName"
      override def getConnection(): Connection = connection
      override def getConnection(autocommit: Boolean): Connection = {
        connection.setAutoCommit(autocommit)
        connection
      }
      override def withConnection[A](block: Connection => A): A = block(connection)
      override def withConnection[A](autocommit: Boolean)(block: Connection => A): A = block(connection)
      override def withTransaction[A](block: Connection => A): A = block(connection)
      override def withTransaction[A](isolationLevel: play.api.db.TransactionIsolationLevel)(block: Connection => A): A = block(connection)
      override def shutdown(): Unit = connection.close()
    }

    try { f(db) } finally { connection.close() }
  }

  private def setupSchema(connection: Connection): Unit = {
    Seq(
      """
        CREATE TABLE Site (
          seq INT AUTO_INCREMENT PRIMARY KEY,
          created DATETIME DEFAULT NOW() NOT NULL,
          updated DATETIME DEFAULT NOW() NOT NULL,
          name VARCHAR(200) NOT NULL
        )
      """,
      """
        CREATE TABLE `User` (
          seq INT AUTO_INCREMENT PRIMARY KEY,
          created DATETIME DEFAULT NOW() NOT NULL,
          updated DATETIME DEFAULT NOW() NOT NULL,
          nickname VARCHAR(32) NOT NULL
        )
      """,
      """
        CREATE TABLE SiteAdmin (
          site INT NOT NULL,
          `user` INT NOT NULL,
          dateInserted DATETIME DEFAULT NOW() NOT NULL,
          PRIMARY KEY (site, `user`),
          CONSTRAINT SiteAdmin_Site_seq_fk FOREIGN KEY (site) REFERENCES Site (seq),
          CONSTRAINT SiteAdmin_User_seq_fk FOREIGN KEY (`user`) REFERENCES `User` (seq)
        )
      """,
      "INSERT INTO Site (seq, name) VALUES (1, 'SiteA')",
      "INSERT INTO Site (seq, name) VALUES (2, 'SiteB')",
      "INSERT INTO `User` (seq, nickname) VALUES (1, 'superadmin')",
      "INSERT INTO `User` (seq, nickname) VALUES (10, 'alice')",
      "INSERT INTO `User` (seq, nickname) VALUES (20, 'bob')",
    ).foreach(sql => SQL(sql).execute()(connection))
  }

  "isSiteAdmin" - {
    "super admin (seq == 1) is always site admin regardless of SiteAdmin table" in withDatabase { implicit db =>
      val superAdminSeq = 1L
      assert(AdminLogic.isSiteAdminBySeq(1, superAdminSeq))
      assert(AdminLogic.isSiteAdminBySeq(2, superAdminSeq))
    }

    "user in SiteAdmin table is site admin for that site only" in withDatabase { implicit db =>
      db.withConnection { implicit c => SiteAdmin.insert(1, 10) }

      assert(AdminLogic.isSiteAdminBySeq(1, 10L))
      assert(!AdminLogic.isSiteAdminBySeq(2, 10L))
    }

    "user not in SiteAdmin table is not site admin" in withDatabase { implicit db =>
      assert(!AdminLogic.isSiteAdminBySeq(1, 20L))
    }
  }
}
