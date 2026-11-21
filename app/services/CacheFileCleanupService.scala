package services

import com.aha00a.commons.utils.StopWatch
import logics.wikis.interpreters.InterpreterVim
import play.api.Logging

import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import scala.jdk.CollectionConverters._
import scala.util.Try

case class CacheCleanupResult(name: String, deletedCount: Int)

/**
 * Removes what the Vim cache no longer needs; run once a month (ApplicationLifecycleHook).
 *
 *  - An entry (`{md5}.html`) nothing has drawn for a year. Its modification time is when it was
 *    last drawn (InterpreterVim.markUsed). A removed entry costs one vi run the next time its block
 *    is drawn, nothing more.
 *  - Anything else in the directory that is not a working directory still in use: the `.txt`, `.sh`
 *    and `.raw.html` every block left until 2026-11-22, and a working directory older than a day,
 *    left by a process that died while vi ran.
 *
 * Until 2026-11-22 this ran weekly, read the access time, and stopped at 1,000 files a run. The
 * access time had been set for every file by one read of the whole directory on 2026-11-21, and
 * anything that reads the files sets it again, so nothing was ever old enough. The cap is gone
 * because the first run has some 21,000 left-over files to remove, all of them by-products.
 */
class CacheFileCleanupService extends Logging {
  private val day: Long = 1000L * 60 * 60 * 24
  val EntryRetentionMillis: Long = 365 * day

  val Interval: Long = 30 * day

  /** Where the last run is recorded: its modification time. In the cache directory, which outlives
    * a release, because a deploy restarts the process nearly every day and a timer in the process
    * would never reach a month. */
  val LastRunMarker = ".last-cleanup"

  def cleanupAllExpiredCaches(): Seq[CacheCleanupResult] =
    cleanupVimCacheIfDue(InterpreterVim.getCacheDir, System.currentTimeMillis()).map(CacheCleanupResult("Vim", _)).toSeq

  /** Runs the cleanup when the last one was a month ago or never; None when it is not yet due. */
  def cleanupVimCacheIfDue(cacheDir: File, now: Long): Option[Int] = {
    val marker = new File(cacheDir, LastRunMarker)
    if (!cacheDir.isDirectory || (marker.exists() && marker.lastModified() > now - Interval)) {
      None
    } else {
      val removed = cleanupVimCache(cacheDir, now)
      Try { if (!marker.exists()) marker.createNewFile(); marker.setLastModified(now) }
      logger.info(s"Vim cache cleanup removed $removed")
      Some(removed)
    }
  }

  def cleanupVimCache(cacheDir: File, now: Long): Int =
    StopWatch(s"CacheFileCleanupService.cleanupVimCache\t$cacheDir") {
      if (!cacheDir.isDirectory) {
        0
      } else {
        val stream = Files.newDirectoryStream(cacheDir.toPath)
        val paths = try stream.iterator().asScala.toList finally stream.close()
        paths.count { path =>
          val name = path.getFileName.toString
          val modified = Try(Files.getLastModifiedTime(path).toMillis).getOrElse(now)
          val remove =
            if (name == LastRunMarker) false
            else if (Files.isDirectory(path)) name.startsWith(InterpreterVim.WorkDirPrefix) && modified < now - day
            else if (name.matches("[0-9a-f]{32}\\.html")) modified < now - EntryRetentionMillis
            else true
          remove && delete(path)
        }
      }
    }

  private def delete(path: Path): Boolean = Try {
    if (Files.isDirectory(path)) Option(path.toFile.listFiles()).toSeq.flatten.foreach(_.delete())
    Files.deleteIfExists(path)
  }.getOrElse {
    logger.warn(s"Failed to delete a Vim cache file: ${path.toAbsolutePath}")
    false
  }
}
