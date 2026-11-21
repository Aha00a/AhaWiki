package com.aha00a.logics

import logics.wikis.ReadablePages
import models.tables.SearchResult
import org.scalatest.freespec.AnyFreeSpec

import java.time.LocalDateTime

/** The two ways a list of pages is cut down to what the reader may read (ReadablePages). The
  * search and the change lists each had two copies of these until 2026-11-21. */
class ReadablePagesSpec extends AnyFreeSpec {
  private def at(day: Int) = LocalDateTime.of(2026, 11, day, 0, 0)
  private def result(name: String, day: Int) = SearchResult(name, at(day), "")
  private val secret: String => Boolean = name => !name.startsWith("Secret")

  "search" - {
    val matches = Seq(result("A", 1), result("SecretB", 3), result("C", 2), result("D", 4))

    "asks for one more than shown, and keeps the readable ones newest first" in {
      var asked = 0
      val (readable, truncated) = ReadablePages.search(n => { asked = n; matches.take(n) }, 3, secret)
      assert(asked === 4)
      assert(readable.map(_.name) === Seq("C", "A"))
      assert(truncated)
    }

    "drops unreadable matches after the cut rather than reaching further" in {
      val (readable, _) = ReadablePages.search(matches.take, 2, secret)
      assert(readable.map(_.name) === Seq("A"))
    }

    "is not truncated when every match fits" in {
      val (readable, truncated) = ReadablePages.search(matches.take, 4, _ => true)
      assert(readable.map(_.name) === Seq("D", "SecretB", "C", "A"))
      assert(!truncated)
    }
  }

  "collect" - {
    val rows = (1 to 10).map(i => if (i % 2 == 0) s"Secret$i" else s"P$i")
    def select(batchSize: Int)(offset: Int): Seq[String] = rows.slice(offset, offset + batchSize)

    "reads further batches until it has as many readable rows as asked" in {
      assert(ReadablePages.collect[String](select(2), 2, 3, identity, secret) === List("P1", "P3", "P5"))
    }

    "stops at a short batch with what it has" in {
      assert(ReadablePages.collect[String](select(4), 4, 10, identity, secret) === List("P1", "P3", "P5", "P7", "P9"))
    }

    "never gives more than asked" in {
      assert(ReadablePages.collect[String](select(10), 10, 2, identity, _ => true) === List("P1", "Secret2"))
    }
  }
}
