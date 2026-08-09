package com.aha00a.models.tables

import anorm.SQL
import com.aha00a.tests.TestApplication
import com.aha00a.tests.TestSchema
import models.tables.Site
import org.scalatest.freespec.AnyFreeSpec

import java.sql.Connection
import java.sql.DriverManager

class SiteSpec extends AnyFreeSpec {
  "selectPublicListed" - {
    "keeps positive publicListedOrder rows and sorts by order descending then seq ascending" in {
      Class.forName("org.h2.Driver")
      val connection = DriverManager.getConnection(TestApplication.h2Url(TestApplication.randomDbName("sites")))
      try {
        TestSchema.create("Site")(connection)
        insertFixture(connection)

        implicit val implicitConnection: Connection = connection

        val allSites = Site.select()
        assert(allSites.find(_.seq == 1).flatMap(_.publicListedOrder) === Some(BigDecimal("10.25")))
        assert(allSites.find(_.seq == 2).flatMap(_.publicListedOrder) === None)

        val publicSites = Site.selectPublicListed()
        assert(publicSites.map(_.seq) === Seq(4, 1, 6))
      } finally {
        connection.close()
      }
    }
  }

  private def insertFixture(connection: Connection): Unit = {
    Seq(
      "INSERT INTO Site (seq, name, abbr, mainDomain, publicListedOrder) VALUES (1, 'VisibleLow', 'VL', 'low.example', 10.25)",
      "INSERT INTO Site (seq, name, abbr, mainDomain, publicListedOrder) VALUES (2, 'HiddenNull', 'HN', 'hidden-null.example', NULL)",
      "INSERT INTO Site (seq, name, abbr, mainDomain, publicListedOrder) VALUES (3, 'HiddenZero', 'HZ', 'hidden-zero.example', 0.00)",
      "INSERT INTO Site (seq, name, abbr, mainDomain, publicListedOrder) VALUES (4, 'VisibleHigh', 'VH', 'high.example', 20.00)",
      "INSERT INTO Site (seq, name, abbr, mainDomain, publicListedOrder) VALUES (5, 'NoDomain', 'ND', '', 30.00)",
      "INSERT INTO Site (seq, name, abbr, mainDomain, publicListedOrder) VALUES (6, 'VisibleTie', 'VT', 'tie.example', 10.25)",
    ).foreach(sql => SQL(sql).execute()(connection))
  }
}
