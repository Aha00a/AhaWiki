package models.tables

import java.sql.Connection
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
object LocalTransaction {
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
