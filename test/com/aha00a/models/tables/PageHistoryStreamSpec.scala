package com.aha00a.models.tables

import anorm.SQL
import com.aha00a.tests.TestApplication
import com.aha00a.tests.TestSchema
import models.tables.Page
import models.tables.Site
import org.scalatest.freespec.AnyFreeSpec

import java.sql.Connection
import java.sql.DriverManager

/** `selectHistoryStream` is what blame reads. It reads a page's revisions in chunks rather than all
  * at once, so the thing to get wrong is the seam: a revision dropped or read twice where one
  * chunk ends and the next begins, or a last chunk that happens to be exactly full. */
class PageHistoryStreamSpec extends AnyFreeSpec {
  private implicit val site: Site = Site(1, "SiteA", "SiteA", "a.example")

  private def withConnection[T](f: Connection => T): T = {
    Class.forName("org.h2.Driver")
    // TestApplication's URL, not a bare one: blame's query joins `User`, which bare H2 reads as a keyword.
    val connection = DriverManager.getConnection(TestApplication.h2Url(TestApplication.randomDbName("history")))
    try {
      TestSchema.createAll()(connection)
      Seq(
        "INSERT INTO Site (seq, name, abbr) VALUES (1, 'SiteA', 'SiteA')",
        "INSERT INTO Site (seq, name, abbr) VALUES (2, 'SiteB', 'SiteB')",
        "INSERT INTO User (seq, nickname) VALUES (1, 'alice')",
      ).foreach(sql => SQL(sql).execute()(connection))
      f(connection)
    } finally {
      connection.close()
    }
  }

  private def insertRevisions(siteSeq: Long, name: String, count: Int)(implicit c: Connection): Unit =
    (1 to count).foreach { revision =>
      SQL(
        s"""INSERT INTO Page (site, name, revision, dateTime, `user`, remoteAddress, comment, content)
           |VALUES ($siteSeq, '$name', $revision, NOW(), 1, '127.0.0.1', '', '$name r$revision')""".stripMargin
      ).execute()
    }

  private def folded(name: String)(implicit c: Connection): Seq[(Long, String)] =
    Page.selectHistoryStream[Vector[(Long, String)]](name, Vector.empty, (acc, p) => acc :+ (p.revision -> p.content))

  "every revision, oldest first, exactly once" - {
    Seq(1, 19, 20, 21, 40, 45).foreach { count =>
      s"$count revisions" in withConnection { implicit c =>
        insertRevisions(1, "P", count)
        assert(folded("P") === (1 to count).map(r => (r.toLong, s"P r$r")))
      }
    }
  }

  "nothing for a page that does not exist" in withConnection { implicit c =>
    assert(folded("Missing").isEmpty)
  }

  "only this page on this site" in withConnection { implicit c =>
    insertRevisions(1, "P", 3)
    insertRevisions(1, "Q", 25)
    insertRevisions(2, "P", 30)
    assert(folded("P").map(_._1) === Seq(1L, 2L, 3L))
  }
}
