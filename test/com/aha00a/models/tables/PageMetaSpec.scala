package com.aha00a.models.tables

import anorm.SQL
import anorm.SqlStringInterpolation
import com.aha00a.tests.TestApplication
import com.aha00a.tests.TestSchema
import models.tables.PageMeta
import models.tables.Site
import org.scalatest.freespec.AnyFreeSpec

import java.sql.Connection
import java.sql.DriverManager

/** Page calculation is asynchronous: it reads the latest revision, and by the time it writes
  * PageMeta another request may have deleted that revision or renamed the page. The foreign key
  * then refuses the row. That used to surface as an ERROR from the actor's supervisor -- twenty in
  * the production log -- for something that is not an error: whoever removed the revision queues
  * a calculation of their own. These specs replay the late write.
  */
class PageMetaSpec extends AnyFreeSpec {
  private implicit val site: Site = Site(1, "SiteA", "SiteA", "site1.example")

  "upsert" - {
    "writes the row and says so when the revision is there" in {
      withConnection { implicit connection =>
        assert(PageMeta.upsert("Foo", 1, None, Some("first"), 5) === true)
        assert(PageMeta.upsert("Foo", 2, None, Some("second"), 6) === true)
        assert(revisionOf("Foo") === Some(2L))
      }
    }

    "says false, without throwing, when the revision was deleted in the meantime" in {
      withConnection { implicit connection =>
        assert(PageMeta.upsert("Foo", 2, None, None, 6) === true)
        assert(PageMeta.upsert("Foo", 3, None, None, 7) === false)
        assert(revisionOf("Foo") === Some(2L))
      }
    }

    "says false when the page was renamed away" in {
      withConnection { implicit connection =>
        assert(PageMeta.upsert("Gone", 1, None, None, 1) === false)
        assert(revisionOf("Gone") === None)
      }
    }
  }

  private def revisionOf(name: String)(implicit connection: Connection): Option[Long] =
    SQL"SELECT revision FROM PageMeta WHERE site = 1 AND name = $name".as(anorm.SqlParser.long("revision").singleOpt)

  private def withConnection(f: Connection => Unit): Unit = {
    Class.forName("org.h2.Driver")
    val connection = DriverManager.getConnection(TestApplication.h2Url(TestApplication.randomDbName("page_meta")))
    try {
      TestSchema.createAll()(connection)
      Seq(
        "INSERT INTO Site (seq, name, abbr) VALUES (1, 'SiteA', 'SiteA')",
        "INSERT INTO `User` (seq, nickname) VALUES (1, 'writer')",
        "INSERT INTO Page (site, name, revision, dateTime, `user`, remoteAddress, comment, content) VALUES (1, 'Foo', 1, NOW(), 1, '127.0.0.1', '', 'one')",
        "INSERT INTO Page (site, name, revision, dateTime, `user`, remoteAddress, comment, content) VALUES (1, 'Foo', 2, NOW(), 1, '127.0.0.1', '', 'two')",
      ).foreach(sql => SQL(sql).execute()(connection))
      f(connection)
    } finally {
      connection.close()
    }
  }
}
