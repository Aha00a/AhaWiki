package models.tables

import anorm._

import java.sql.Connection
import java.time.LocalDateTime

/**
 * Deletes the oldest rows of a log-shaped table once they pass their retention.
 *
 * Three tables — `AccessLog`, `IpDeny`, `UserViewHistory` — had this query written out
 * separately, down to the same subselect and watermark, differing only in the table name and
 * the interval. They were formatted three different ways, which is what a copy looks like
 * after a while.
 *
 * The batching is the point. It considers only the `limit` oldest rows and deletes up to the
 * newest of those that has expired, so shortening a retention period does not turn into one
 * enormous delete. Convergence takes several runs instead.
 *
 * When nothing in that window has expired the subselect yields NULL, `seq < NULL` matches
 * nothing, and the call deletes zero rows. That is the intended idle case.
 */
object ExpiredRows {
  private val tableNamePattern = "^[A-Za-z][A-Za-z0-9_]*$"

  /**
   * @param table  a literal table name from this package — never anything a request supplied,
   *               because a table name cannot be a bound parameter and has to be interpolated.
   */
  def deleteInsertedBefore(table: String, threshold: LocalDateTime, limit: Int)(implicit connection: Connection): Int = {
    require(table.matches(tableNamePattern), s"suspicious table name: $table")
    SQL(
      s"""
        DELETE FROM $table
        WHERE seq < (
          SELECT MAX(seq)
          FROM (
            SELECT seq, dateInserted
            FROM $table
            ORDER BY seq
            LIMIT {limit}
          ) T
          WHERE T.dateInserted < {threshold}
        )
      """
    ).on("limit" -> limit, "threshold" -> threshold).executeUpdate()
  }
}
