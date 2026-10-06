package com.aha00a.logics

import com.aha00a.tests.TestApplication.TestSyncCacheApi
import logics.AhaWikiCache
import org.scalatest.freespec.AnyFreeSpec
import play.api.Environment
import play.api.Mode

import scala.collection.mutable
import scala.reflect.ClassTag

/** `AhaWikiCache` keeps each value decoded in the process and asks Redis only for a version key.
  * Production runs two instances on one Redis, so most of what matters here is between two
  * `AhaWikiCache`s sharing one cache: one must see what the other invalidates or writes, at its
  * very next read, exactly as when every read fetched the value. */
class AhaWikiCacheSpec extends AnyFreeSpec {
  private val key = "Spec:list"
  private implicit val i: Int = 1

  private class CountingCache extends TestSyncCacheApi {
    val valueReads: mutable.Buffer[String] = mutable.Buffer.empty
    @volatile var failing: Boolean = false

    override def get[T](key: String)(implicit ct: ClassTag[T]): Option[T] = {
      if (failing) throw new RuntimeException("redis down")
      if (!key.endsWith(":version")) valueReads += key
      super.get[T](key)
    }
  }

  private class Origin(var value: Seq[String]) {
    var computed: Int = 0
  }

  private def entity(cache: AhaWikiCache, source: Origin) = new cache.CacheEntity[Seq[String], Int] {
    override def key()(implicit i: Int): String = AhaWikiCacheSpec.this.key
    override def orElse()(implicit i: Int): Seq[String] = { source.computed += 1; source.value }
  }

  private def instance(redis: CountingCache): AhaWikiCache = new AhaWikiCache(redis, Environment.simple(mode = Mode.Test))

  "a second read neither fetches nor decodes the value" in {
    val redis = new CountingCache
    val source = new Origin(Seq("a", "b"))
    val e = entity(instance(redis), source)
    val first = e.get()
    val second = e.get()
    assert(first === Seq("a", "b"))
    assert(second eq first)
    assert(source.computed === 1)
    assert(redis.valueReads.size === 1)
  }

  "the other instance reads what this one computed, without computing it again" in {
    val redis = new CountingCache
    val source = new Origin(Seq("a"))
    entity(instance(redis), source).get()
    assert(entity(instance(redis), source).get() === Seq("a"))
    assert(source.computed === 1)
  }

  "an invalidation on the other instance is seen at the next read" in {
    val redis = new CountingCache
    val source = new Origin(Seq("old"))
    val a = entity(instance(redis), source)
    val b = entity(instance(redis), source)
    assert(a.get() === Seq("old"))
    source.value = Seq("new")
    b.invalidate()
    assert(a.get() === Seq("new"))
  }

  "a value written before version keys existed is read, and fetched again until it is rewritten" in {
    val redis = new CountingCache
    redis.set(key, """["legacy"]""", scala.concurrent.duration.Duration.Inf)
    val source = new Origin(Seq("computed"))
    val e = entity(instance(redis), source)
    assert(e.get() === Seq("legacy"))
    assert(e.get() === Seq("legacy"))
    assert(redis.valueReads.size === 2)
    assert(source.computed === 0)
  }

  "when Redis fails, the last decoded value is served" in {
    val redis = new CountingCache
    val source = new Origin(Seq("kept"))
    val e = entity(instance(redis), source)
    assert(e.get() === Seq("kept"))
    redis.failing = true
    assert(e.get() === Seq("kept"))
  }

  "when Redis fails and nothing was decoded, the failure is thrown" in {
    val redis = new CountingCache
    redis.failing = true
    assertThrows[RuntimeException](entity(instance(redis), new Origin(Seq("x"))).get())
  }
}
