package com.aha00a.models.tables

import anorm.SQL
import anorm.SqlParser
import com.aha00a.tests.TestApplication
import com.aha00a.tests.TestSchema
import models.tables.ExpiredRows
import org.scalatest.freespec.AnyFreeSpec

import java.sql.Connection
import java.sql.DriverManager
import java.time.LocalDateTime

/** What ExpiredRows.deleteInsertedBefore deletes: among the `limit` oldest rows, everything below
  * the newest one that has expired -- that row itself stays for the next run -- and nothing when
  * none of them has.
  *
  * It used to be one DELETE with the cutoff as a subquery, which MariaDB runs as a scan of the
  * whole table. These specs pin down the rows it deletes, which the split into a SELECT of the
  * cutoff and a DELETE below it keeps as they were. H2 cannot show the plan; Dev Database has the
  * EXPLAIN from production. */
class ExpiredRowsSpec extends AnyFreeSpec {
  private val now = LocalDateTime.of(2026, 10, 4, 12, 0)
  private val threshold = now.minusDays(90)

  "deleteInsertedBefore" - {
    "deletes below the newest expired row among the oldest `limit`, and leaves that row" in {
      withRows(Seq(200, 150, 120, 91, 10, 5)) { implicit connection =>
        // Rows 1-4 are older than 90 days. With a window of all six the newest expired is row 4, so
        // rows 1-3 go and row 4 waits for the next run.
        assert(ExpiredRows.deleteInsertedBefore("IpDeny", threshold, 10) === 3)
        assert(remaining() === Seq(4, 5, 6))
      }
    }

    "looks only at the `limit` oldest rows" in {
      withRows(Seq(200, 150, 120, 91, 10, 5)) { implicit connection =>
        // A window of two sees rows 1 and 2; the newest expired there is row 2.
        assert(ExpiredRows.deleteInsertedBefore("IpDeny", threshold, 2) === 1)
        assert(remaining() === Seq(2, 3, 4, 5, 6))
      }
    }

    "deletes nothing when nothing in the window has expired" in {
      withRows(Seq(30, 20, 10)) { implicit connection =>
        assert(ExpiredRows.deleteInsertedBefore("IpDeny", threshold, 10) === 0)
        assert(remaining() === Seq(1, 2, 3))
      }
    }

    "deletes nothing from an empty table" in {
      withRows(Seq()) { implicit connection =>
        assert(ExpiredRows.deleteInsertedBefore("IpDeny", threshold, 10) === 0)
      }
    }

    "refuses a table name that is not a plain identifier" in {
      withRows(Seq()) { implicit connection =>
        assertThrows[IllegalArgumentException](ExpiredRows.deleteInsertedBefore("IpDeny; DROP TABLE Page", threshold, 10))
      }
    }
  }

  private def remaining()(implicit connection: Connection): Seq[Int] =
    SQL("SELECT seq FROM IpDeny ORDER BY seq").as(SqlParser.int("seq").*)

  /** IpDeny rows with seq 1, 2, ... inserted the given number of days before `now`. */
  private def withRows(daysAgo: Seq[Int])(f: Connection => Unit): Unit = {
    Class.forName("org.h2.Driver")
    val connection = DriverManager.getConnection(TestApplication.h2Url(TestApplication.randomDbName("expired_rows")))
    try {
      TestSchema.createAll()(connection)
      daysAgo.zipWithIndex.foreach { case (days, i) =>
        SQL("INSERT INTO IpDeny (seq, dateInserted, ip, reason) VALUES ({seq}, {at}, '127.0.0.1', 'spec')")
          .on("seq" -> (i + 1), "at" -> now.minusDays(days.toLong))
          .execute()(connection)
      }
      f(connection)
    } finally connection.close()
  }
}
