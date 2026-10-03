package models.tables

import play.api.Logging

import java.sql.Connection
import java.sql.SQLTransactionRollbackException
import java.sql.Savepoint

/**
 * Runs a block atomically on a connection that may or may not already be in a transaction.
 *
 * Callers receive their connection from `Database.withConnection`, which leaves autocommit
 * on, but the same methods are also reached from inside `withTransaction`. So the block has
 * to work either way: it opens a transaction when there is none, and takes a savepoint when
 * there already is one.
 *
 * The nested case used to differ per table — `Page` took a savepoint while `User` and
 * `UserMerge` ran the block bare and let the exception through with the block's partial
 * writes still pending. Both rethrow, so an outer handler that rolls everything back saw no
 * difference; an outer handler that catches and continues did. The savepoint is kept here,
 * because it is the only version that leaves the connection in a state the caller can
 * describe.
 */
object LocalTransaction extends Logging {
  /**
   * The same, and when the database picks the block as a deadlock victim, the block again -- up to
   * three runs in all. Only for a block that can be run twice with the same result, which is what
   * recalculating derived rows is.
   *
   * Two instances recalculate pages on their own traffic, and two pages similar to each other
   * write the same CalculatedCosineSimilarity rows. With each recalculation one transaction they
   * can deadlock: none in the 53 days of log before 2026-10-03, one within the hour after. The
   * victim's exception stopped the rest of that page's calculation, links included, so it is
   * retried rather than only logged. MySQL's own message for it is "try restarting transaction".
   *
   * Not retried inside a transaction the caller opened: a deadlock rolls back all of it, and
   * running this block alone again would commit the caller's work without its earlier half.
   *
   * Each retry leaves one INFO line, "deadlock victim, running again". A retry that succeeds is
   * otherwise invisible, and that line is the only way to tell how often this happens.
   */
  def retryingDeadlock[T](f: => T)(implicit connection: Connection): T = {
    def run(runsLeft: Int): T =
      try apply(f)
      catch {
        case e: SQLTransactionRollbackException if runsLeft > 1 && connection.getAutoCommit =>
          logger.info(s"LocalTransaction: deadlock victim, running again (${runsLeft - 1} more at most): ${e.getMessage}")
          run(runsLeft - 1)
      }
    run(3)
  }

  def apply[T](f: => T)(implicit connection: Connection): T = {
    if (connection.getAutoCommit) {
      connection.setAutoCommit(false)
      try {
        val result = f
        connection.commit()
        result
      } catch {
        case e: Throwable =>
          connection.rollback()
          throw e
      } finally {
        connection.setAutoCommit(true)
      }
    } else {
      val savepoint: Savepoint = connection.setSavepoint()
      try {
        f
      } catch {
        case e: Throwable =>
          connection.rollback(savepoint)
          throw e
      }
    }
  }
}
