package logics.wikis

import models.tables.SearchResult

import java.time.LocalDateTime
import scala.annotation.tailrec

/**
 * Lists of pages cut down to what the reader may read. Each shape had two copies until 2026-11-21:
 * the search in `Search` and `ApiV1`, the change list in `Api` and `ApiV1`. The copies still
 * agreed, which is the moment to merge them; the queries stay with their callers, since what they
 * select differs.
 */
object ReadablePages {

  /**
   * A search's matches the reader may read, newest first, and whether the limit cut the matches
   * short. `find` is asked for one more than `max` to learn that.
   *
   * Unreadable matches are dropped after the cut, not before, so a reader who can read little sees
   * fewer than `max` rather than the search reaching further for them. `truncated` is about the
   * matches, readable or not: it says there were more, not that more of them are readable.
   */
  def search(find: Int => Seq[SearchResult], max: Int, isReadable: String => Boolean): (Seq[SearchResult], Boolean) = {
    val found = find(max + 1)
    val readable = found
      .take(max)
      .filter(r => isReadable(r.name))
      .sortBy(_.dateTime)(Ordering[LocalDateTime].reverse)
    (readable, found.size > max)
  }

  /**
   * Up to `limit` readable rows, reading `select(offset)` a batch at a time until there are enough
   * or a short batch says the table has run out. Unlike the search, a change list reaches further
   * when rows are unreadable: it is asked for a number of changes, and it gives that many if they
   * exist.
   */
  @tailrec
  def collect[A](select: Int => Seq[A], batchSize: Int, limit: Int, name: A => String, isReadable: String => Boolean, offset: Int = 0, acc: List[A] = Nil): List[A] =
    if (acc.size >= limit) {
      acc.take(limit)
    } else {
      val rows = select(offset)
      val next = acc ++ rows.filter(row => isReadable(name(row)))
      if (rows.size < batchSize) next.take(limit) else collect(select, batchSize, limit, name, isReadable, offset + rows.size, next)
    }
}
