package com.aha00a.models.tables

import com.aha00a.tests.TestSchema
import anorm.SQL
import models.tables.SiteAdmin
import org.scalatest.freespec.AnyFreeSpec

import java.sql.Connection
import java.sql.DriverManager

class SiteAdminSpec extends AnyFreeSpec {
  private def withConnection[T](f: Connection => T): T = {
    Class.forName("org.h2.Driver")
    val dbName = s"siteadmin_${java.util.UUID.randomUUID().toString.replace("-", "")}"
    val connection = DriverManager.getConnection(s"jdbc:h2:mem:$dbName;MODE=MySQL;DB_CLOSE_DELAY=-1")
    try {
      setupSchema(connection)
      f(connection)
    } finally {
      connection.close()
    }
  }

  private def setupSchema(connection: Connection): Unit = {
    TestSchema.createAll()(connection)
    Seq(
      "INSERT INTO Site (seq, name, abbr) VALUES (1, 'SiteA', 'SiteA')",
      "INSERT INTO Site (seq, name, abbr) VALUES (2, 'SiteB', 'SiteB')",
      "INSERT INTO `User` (seq, nickname) VALUES (10, 'alice')",
      "INSERT INTO `User` (seq, nickname) VALUES (20, 'bob')",
      "INSERT INTO `User` (seq, nickname) VALUES (30, 'carol')",
    ).foreach(sql => SQL(sql).execute()(connection))
  }

  "exists" - {
    "returns false when no admins" in withConnection { implicit c =>
      assert(!SiteAdmin.exists(1, 10))
    }

    "returns true after insert" in withConnection { implicit c =>
      SiteAdmin.insert(1, 10)
      assert(SiteAdmin.exists(1, 10))
    }

    "is scoped to site — same user on different site returns false" in withConnection { implicit c =>
      SiteAdmin.insert(1, 10)
      assert(!SiteAdmin.exists(2, 10))
    }
  }

  "selectBySite" - {
    "returns empty when no admins" in withConnection { implicit c =>
      assert(SiteAdmin.selectBySite(1).isEmpty)
    }

    "returns only admins for the given site" in withConnection { implicit c =>
      SiteAdmin.insert(1, 10)
      SiteAdmin.insert(1, 20)
      SiteAdmin.insert(2, 30)

      val site1Admins = SiteAdmin.selectBySite(1)
      assert(site1Admins.map(_.user).toSet === Set(10L, 20L))

      val site2Admins = SiteAdmin.selectBySite(2)
      assert(site2Admins.map(_.user) === Seq(30L))
    }
  }

  "insert" - {
    "inserts a new site admin row" in withConnection { implicit c =>
      val affected = SiteAdmin.insert(1, 10)
      assert(affected === 1)
      assert(SiteAdmin.exists(1, 10))
    }

    "does not allow duplicate (site, user)" in withConnection { implicit c =>
      SiteAdmin.insert(1, 10)
      assertThrows[Exception] {
        SiteAdmin.insert(1, 10)
      }
    }
  }

  "delete" - {
    "removes the row and returns 1" in withConnection { implicit c =>
      SiteAdmin.insert(1, 10)
      val deleted = SiteAdmin.delete(1, 10)
      assert(deleted === 1)
      assert(!SiteAdmin.exists(1, 10))
    }

    "returns 0 when row does not exist" in withConnection { implicit c =>
      val deleted = SiteAdmin.delete(1, 10)
      assert(deleted === 0)
    }

    "only removes the matching (site, user) row" in withConnection { implicit c =>
      SiteAdmin.insert(1, 10)
      SiteAdmin.insert(1, 20)
      SiteAdmin.delete(1, 10)

      assert(!SiteAdmin.exists(1, 10))
      assert(SiteAdmin.exists(1, 20))
    }
  }
}
