package com.aha00a.models.tables

import com.aha00a.tests.TestSchema
import anorm.SQL
import anorm.SqlStringInterpolation
import models.tables.CalculatedCosineSimilarity
import models.tables.Site
import org.scalatest.freespec.AnyFreeSpec

import java.sql.Connection
import java.sql.DriverManager

class CalculatedCosineSimilaritySpec extends AnyFreeSpec {
  "recalc" - {
    "stores same-site and cross-site similarities in the site-aware schema" in {
      Class.forName("org.h2.Driver")
      val connection = DriverManager.getConnection("jdbc:h2:mem:ccs;MODE=MySQL;DB_CLOSE_DELAY=-1")
      try {
        setupSchema(connection)
        insertFixture(connection)

        implicit val implicitConnection: Connection = connection
        implicit val site: Site = Site(1, "SiteOne", "S1", "site1.example")

        CalculatedCosineSimilarity.recalc("Foo")

        val sameSite = CalculatedCosineSimilarity.selectSameSite("Foo")
        assert(sameSite.map(c => (c.site1, c.name1, c.site2, c.name2)).contains((1L, "Foo", 1L, "Bar")))

        val crossSite = CalculatedCosineSimilarity.selectCrossSite("Foo")
        assert(crossSite.map(c => (c.site1, c.name1, c.site2, c.name2)).contains((1L, "Foo", 2L, "Baz")))

        val reverseCrossSite = SQL"""
          SELECT COUNT(*) count_value
          FROM CalculatedCosineSimilarity
          WHERE site1 = 2 AND name1 = 'Baz' AND site2 = 1 AND name2 = 'Foo'
        """.as(anorm.SqlParser.long("count_value").single)
        assert(reverseCrossSite === 1L)
      } finally {
        connection.close()
      }
    }
  }

  private def setupSchema(connection: Connection): Unit = {
    TestSchema.create("PageMeta", "CalculatedTerm", "CalculatedTermFrequency", "CalculatedTermFrequencyNorm", "CalculatedCosineSimilarity")(connection)
  }

  private def insertFixture(connection: Connection): Unit = {
    Seq(
      "INSERT INTO PageMeta (site, name, revision) VALUES (1, 'Foo', 1)",
      "INSERT INTO PageMeta (site, name, revision) VALUES (1, 'Bar', 1)",
      "INSERT INTO PageMeta (site, name, revision) VALUES (2, 'Baz', 1)",
      "INSERT INTO CalculatedTerm (seq, term) VALUES (1, 'alpha')",
      "INSERT INTO CalculatedTerm (seq, term) VALUES (2, 'beta')",
      "INSERT INTO CalculatedTermFrequency (site, name, term, frequency) VALUES (1, 'Foo', 1, 10)",
      "INSERT INTO CalculatedTermFrequency (site, name, term, frequency) VALUES (1, 'Foo', 2, 1)",
      "INSERT INTO CalculatedTermFrequency (site, name, term, frequency) VALUES (1, 'Bar', 1, 9)",
      "INSERT INTO CalculatedTermFrequency (site, name, term, frequency) VALUES (2, 'Baz', 1, 8)",
      """
        INSERT INTO CalculatedTermFrequencyNorm (site, name, norm)
        SELECT site, name, SQRT(SUM(frequency * frequency))
        FROM CalculatedTermFrequency
        GROUP BY site, name
      """,
    ).foreach(sql => SQL(sql).execute()(connection))
  }
}
