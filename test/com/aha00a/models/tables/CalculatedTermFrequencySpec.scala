package com.aha00a.models.tables

import anorm.SQL
import anorm.SqlStringInterpolation
import com.aha00a.tests.TestSchema
import models.tables.CalculatedTerm
import models.tables.CalculatedTermFrequency
import models.tables.Site
import org.scalatest.freespec.AnyFreeSpec

import java.sql.Connection
import java.sql.DriverManager

/** Two instances recalculate the same page concurrently; each does delete-then-insert with no
  * lock between them. These specs replay the loser's turn: the rows it is about to write are
  * already there. It used to die on the duplicate key — the recurring BatchUpdateException in
  * the production log — and the fix is that the latest calculation simply overwrites.
  */
class CalculatedTermFrequencySpec extends AnyFreeSpec {
  "insert" - {
    "writing the same page twice does not throw, and the later frequencies win" in {
      withConnection { implicit connection =>
        implicit val site: Site = Site(1, "SiteA", "SiteA", "site1.example")
        CalculatedTermFrequency.insert("Foo", Seq((1L, 10), (2L, 1)))
        CalculatedTermFrequency.insert("Foo", Seq((1L, 7), (2L, 2)))

        val frequencies = SQL"""
          SELECT term, frequency FROM CalculatedTermFrequency WHERE site = 1 AND name = 'Foo'
        """.as((anorm.SqlParser.long("term") ~ anorm.SqlParser.int("frequency")).*)
          .map(anorm.SqlParser.flatten)
        assert(frequencies.toMap === Map(1L -> 7, 2L -> 2))
      }
    }
  }

  "replace" - {
    // A page view recalculates the page one time in ten, and a reader of SimilarPages used to see
    // whatever lay between the delete and the insert: on 2026-10-02 a canary rendered a page's
    // similar-page terms with some missing, while another instance rendered them whole. A write
    // that fails half-way shows the same thing more plainly -- the old rows have to survive it.
    "is all or nothing: a write that fails half-way leaves the previous frequencies" in {
      withConnection { implicit connection =>
        implicit val site: Site = Site(1, "SiteA", "SiteA", "site1.example")
        CalculatedTermFrequency.replace("Foo", Seq((1L, 10), (2L, 1)))
        assertThrows[java.sql.SQLException](CalculatedTermFrequency.replace("Foo", Seq((1L, 3), (999L, 1))))
        assert(frequencies("Foo") === Map(1L -> 10, 2L -> 1))
      }
    }

    "drops terms the page no longer has" in {
      withConnection { implicit connection =>
        implicit val site: Site = Site(1, "SiteA", "SiteA", "site1.example")
        CalculatedTermFrequency.replace("Foo", Seq((1L, 10), (2L, 1)))
        CalculatedTermFrequency.replace("Foo", Seq((2L, 4)))
        assert(frequencies("Foo") === Map(2L -> 4))
      }
    }
  }

  private def frequencies(name: String)(implicit connection: Connection): Map[Long, Int] =
    SQL"""
      SELECT term, frequency FROM CalculatedTermFrequency WHERE site = 1 AND name = $name
    """.as((anorm.SqlParser.long("term") ~ anorm.SqlParser.int("frequency")).*)
      .map(anorm.SqlParser.flatten).toMap

  "CalculatedTerm.insert" - {
    "inserting a term that lost the select-then-insert race keeps the winner's seq" in {
      withConnection { implicit connection =>
        implicit val site: Site = Site(1, "SiteA", "SiteA", "site1.example")
        val first = CalculatedTerm.ensureSeq("gamma")
        CalculatedTerm.insert("gamma")
        val second = CalculatedTerm.ensureSeq("gamma")
        assert(first === second)
        assert(first.isDefined)
      }
    }
  }

  private def withConnection(f: Connection => Unit): Unit = {
    Class.forName("org.h2.Driver")
    val connection = DriverManager.getConnection("jdbc:h2:mem:ctf;MODE=MySQL;DB_CLOSE_DELAY=-1")
    try {
      TestSchema.createAll()(connection)
      Seq(
        "INSERT INTO Site (seq, name, abbr) VALUES (1, 'SiteA', 'SiteA')",
        "INSERT INTO CalculatedTerm (seq, term) VALUES (1, 'alpha')",
        "INSERT INTO CalculatedTerm (seq, term) VALUES (2, 'beta')",
        // The norm row refers to PageMeta, which refers to the page's revision and its author.
        "INSERT INTO `User` (seq, nickname) VALUES (1, 'writer')",
        "INSERT INTO Page (site, name, revision, dateTime, `user`, remoteAddress, comment, content) VALUES (1, 'Foo', 1, NOW(), 1, '127.0.0.1', '', 'alpha beta')",
        "INSERT INTO PageMeta (site, name, revision) VALUES (1, 'Foo', 1)",
      ).foreach(sql => SQL(sql).execute()(connection))
      f(connection)
    } finally {
      SQL("DROP ALL OBJECTS").execute()(connection)
      connection.close()
    }
  }
}
