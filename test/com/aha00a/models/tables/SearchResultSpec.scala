package com.aha00a.models.tables

import models.tables.SearchResult
import org.scalatest.freespec.AnyFreeSpec

import java.time.LocalDateTime

/** `SearchResult.summarise` builds the excerpt a search result shows. `Page.pageSearch` itself is
  * MySQL-only (see AccessControlFilteringSpec), so this is the part of search a spec can reach. */
class SearchResultSpec extends AnyFreeSpec {
  private def summarise(content: String, q: String) =
    SearchResult("P", LocalDateTime.of(2026, 10, 7, 0, 0), content).summarise(q)

  private def shownLineNumbers(content: String, q: String): Seq[Int] =
    summarise(content, q).summary.flatten.map(_._1)

  "matches case-insensitively and shows three lines of context" in {
    val content = (1 to 20).map(i => if (i == 10) "the Needle here" else s"line $i").mkString("\n")
    assert(shownLineNumbers(content, "needle") === (7 to 13))
    assert(summarise(content, "needle").hitLinesNotShown === 0)
  }

  "reads the query literally, not as a pattern" in {
    val content = "a.c\nabc\n(x)"
    assert(shownLineNumbers("a.c\n1\n2\n3\n4\n5\n6\n7\nabc", "a.c") === (1 to 4))
    assert(shownLineNumbers(content, "(x)").contains(3))
  }

  "shows at most MaxHitLinesPerPage matching lines and counts the rest" in {
    val max = SearchResult.MaxHitLinesPerPage
    val content = (1 to max + 7).map(i => s"hit $i").mkString("\n")
    val summary = summarise(content, "hit")
    assert(summary.hitLinesNotShown === 7)
    // The last shown match is line `max`; its context reaches three lines further.
    assert(summary.summary.flatten.map(_._1).max === max + 3)
  }
}
