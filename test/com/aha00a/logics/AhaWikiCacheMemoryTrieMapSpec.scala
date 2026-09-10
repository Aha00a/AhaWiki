package com.aha00a.logics

import logics.AhaWikiCacheMemoryTrieMap
import org.scalatest.freespec.AnyFreeSpec

import scala.concurrent.duration._

/** The age limit is what carries a permission change to the instance that did not handle it --
  * see AhaWikiCacheMemoryTrieMap.AccessDecisionMaxAge -- so it is pinned here, with a clock the
  * test moves by hand rather than one it waits on. */
class AhaWikiCacheMemoryTrieMapSpec extends AnyFreeSpec {
  private class Fixture(maxAge: Option[FiniteDuration]) {
    var now: Long = 0L
    var loads: Int = 0
    val map = new AhaWikiCacheMemoryTrieMap[String, Int](maxAge = maxAge, nanoTime = () => now)
    def get(): Int = map.getOrElseUpdate("site") { loads += 1; loads }
    def advance(by: FiniteDuration): Unit = now += by.toNanos
  }

  "with maxAge" - {
    "an entry is served from memory while it is younger than that" in {
      val f = new Fixture(Some(10.seconds))
      assert(f.get() === 1)
      f.advance(9.seconds)
      assert(f.get() === 1)
      assert(f.loads === 1)
    }

    "and loaded again on the first read once it is that old" in {
      val f = new Fixture(Some(10.seconds))
      f.get()
      f.advance(10.seconds)
      assert(f.get() === 2)
      assert(f.get() === 2, "the reloaded entry is fresh again")
      assert(f.loads === 2)
    }

    "values shows what is cached, not how it is kept" in {
      val f = new Fixture(Some(10.seconds))
      f.get()
      assert(f.map.values.toList === List(1))
    }
  }

  "without maxAge an entry stays until it is invalidated, however old" in {
    val f = new Fixture(None)
    f.get()
    f.advance(365.days)
    assert(f.get() === 1)
    f.map.invalidate("site")
    assert(f.get() === 2)
  }

  "the access-deciding caches get a bound measured in seconds" in {
    // Six hours was the old effective bound, through a scheduled clear. Anything near that again
    // is the bug this constant exists to prevent.
    assert(AhaWikiCacheMemoryTrieMap.AccessDecisionMaxAge <= 1.minute)
  }
}
