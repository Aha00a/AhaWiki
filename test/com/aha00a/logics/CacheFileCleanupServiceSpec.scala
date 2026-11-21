package com.aha00a.logics

import org.scalatest.freespec.AnyFreeSpec
import services.CacheFileCleanupService

import java.io.File
import java.nio.file.Files

/** What the monthly Vim cache cleanup removes and keeps (CacheFileCleanupService). */
class CacheFileCleanupServiceSpec extends AnyFreeSpec {
  private val day = 24L * 60 * 60 * 1000
  private val now = 1_800_000_000_000L
  private val md5 = "0123456789abcdef0123456789abcdef"

  private def withDir[A](f: File => A): A = {
    val dir = Files.createTempDirectory("vim-cleanup").toFile
    try f(dir) finally {
      Option(dir.listFiles()).toSeq.flatten.foreach { f => Option(f.listFiles()).toSeq.flatten.foreach(_.delete()); f.delete() }
      dir.delete()
    }
  }

  private def file(dir: File, name: String, age: Long): File = {
    val f = new File(dir, name)
    Files.write(f.toPath, "x".getBytes)
    f.setLastModified(now - age)
    f
  }

  "cleanupVimCache" - {
    "removes an entry nobody has drawn for a year and keeps one drawn since" in withDir { dir =>
      val old = file(dir, s"$md5.html", 366 * day)
      val used = file(dir, "fedcba9876543210fedcba9876543210.html", 364 * day)
      assert(new CacheFileCleanupService().cleanupVimCache(dir, now) === 1)
      assert(!old.exists())
      assert(used.exists())
    }

    "removes the by-products every block left until 2026-11-22, however new" in withDir { dir =>
      val left = Seq(".txt", ".sh", ".raw.html").map(ext => file(dir, md5 + ext, 0))
      assert(new CacheFileCleanupService().cleanupVimCache(dir, now) === 3)
      assert(left.forall(!_.exists()))
    }

    "removes a working directory a dead process left, but not one in use" in withDir { dir =>
      def work(name: String, age: Long): File = {
        val w = new File(dir, name); w.mkdir(); file(w, "in.txt", 0); w.setLastModified(now - age); w
      }
      val stale = work("work-1", 2 * day)
      val busy = work("work-2", 60 * 1000)
      assert(new CacheFileCleanupService().cleanupVimCache(dir, now) === 1)
      assert(!stale.exists())
      assert(busy.exists())
    }
  }

  "cleanupVimCacheIfDue" - {
    "runs once, then not again until a month has passed, across restarts" in withDir { dir =>
      val service = new CacheFileCleanupService()
      file(dir, md5 + ".sh", 0)
      assert(service.cleanupVimCacheIfDue(dir, now) === Some(1))
      assert(new File(dir, service.LastRunMarker).exists())
      file(dir, md5 + ".sh", 0)
      assert(new CacheFileCleanupService().cleanupVimCacheIfDue(dir, now + 29 * day) === None)
      assert(new CacheFileCleanupService().cleanupVimCacheIfDue(dir, now + 31 * day) === Some(1))
    }
  }
}
