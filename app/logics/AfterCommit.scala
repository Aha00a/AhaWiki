package logics

import play.api.Logging
import play.api.db.Database

import java.sql.Connection
import scala.collection.mutable.ListBuffer
import scala.util.control.NonFatal

/**
 * What a transaction decides to do once it has committed: queue a page calculation, empty a cache,
 * send a notification.
 *
 * Until 2026-11-18 the paths that rename and delete pages did these inside the transaction. The
 * calculation runs on another connection, so it could read the page before the rename committed
 * and find nothing under the new name; an emptied cache could be filled again by another request
 * with the rows as they were before the commit, and kept for its 5 to 30 minutes. And the
 * transaction's own context holds its connection (ContextSite.holding), so a cache filled inside
 * it holds rows nobody else can see yet -- emptying it after the commit is what makes that safe.
 *
 * What [[apply]] registers does not run if the block throws: the transaction rolled back, and
 * there is nothing to calculate or announce. What [[always]] registers runs either way -- emptying
 * a cache, which a rolled-back block may have filled with rows that were never committed. One task
 * that throws is logged and does not stop the others: the change is committed by then, and an
 * answer of 500 would say it was not.
 */
final class AfterCommit private () {
  private val onCommit = ListBuffer[() => Unit]()
  private val onEnd = ListBuffer[() => Unit]()

  def apply(task: => Unit): Unit = onCommit += (() => task)

  def always(task: => Unit): Unit = onEnd += (() => task)
}

object AfterCommit extends Logging {
  def withTransaction[A](database: Database)(block: AfterCommit => Connection => A): A = {
    val after = new AfterCommit
    val result =
      try database.withTransaction(block(after))
      finally run(after.onEnd)
    run(after.onCommit)
    result
  }

  private def run(tasks: Iterable[() => Unit]): Unit = tasks.foreach { task =>
    try task()
    catch { case NonFatal(e) => logger.error("AfterCommit: a task failed after the transaction", e) }
  }
}
