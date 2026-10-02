package com.aha00a.models.tables

import com.aha00a.tests.TestApplication
import models.tables.LocalTransaction
import org.scalatest.freespec.AnyFreeSpec

import java.sql.Connection
import java.sql.DriverManager
import java.sql.SQLException
import java.sql.SQLTransactionRollbackException

/** Two instances recalculate overlapping derived rows, and since each recalculation became one
  * transaction (2026-10-03) the database sometimes picks one of them as a deadlock victim. It says
  * what to do about that in the message -- "try restarting transaction" -- and for derived data
  * running the block again is always right. These specs stand in for the database with a block
  * that fails the way a victim does.
  */
class LocalTransactionSpec extends AnyFreeSpec {
  private def deadlock = new SQLTransactionRollbackException("Deadlock found when trying to get lock; try restarting transaction", "40001", 1213)

  "retryingDeadlock" - {
    "runs the block again when it was the deadlock victim, and returns the second result" in {
      withConnection { implicit connection =>
        var runs = 0
        val result = LocalTransaction.retryingDeadlock {
          runs += 1
          if (runs == 1) throw deadlock
          "written"
        }
        assert(result === "written")
        assert(runs === 2)
        assert(connection.getAutoCommit)
      }
    }

    "gives up after three runs and lets the deadlock through" in {
      withConnection { implicit connection =>
        var runs = 0
        assertThrows[SQLTransactionRollbackException](LocalTransaction.retryingDeadlock { runs += 1; throw deadlock })
        assert(runs === 3)
        assert(connection.getAutoCommit)
      }
    }

    "does not run the block again for any other failure" in {
      withConnection { implicit connection =>
        var runs = 0
        assertThrows[SQLException](LocalTransaction.retryingDeadlock { runs += 1; throw new SQLException("something else") })
        assert(runs === 1)
      }
    }

    // A deadlock rolls back the whole transaction, not the savepoint, so the caller's earlier
    // writes are gone too: running only this block again would commit half of the caller's work.
    "does not run the block again inside a transaction the caller opened" in {
      withConnection { implicit connection =>
        connection.setAutoCommit(false)
        var runs = 0
        assertThrows[SQLTransactionRollbackException](LocalTransaction.retryingDeadlock { runs += 1; throw deadlock })
        assert(runs === 1)
      }
    }
  }

  private def withConnection(f: Connection => Unit): Unit = {
    Class.forName("org.h2.Driver")
    val connection = DriverManager.getConnection(TestApplication.h2Url(TestApplication.randomDbName("local_transaction")))
    try f(connection) finally connection.close()
  }
}
