package services

import play.api.Logging

import java.io.File
import java.nio.file.Files
import java.nio.file.attribute.BasicFileAttributes

case class CacheCleanupResult(name: String, deletedCount: Int)

class CacheFileCleanupService extends Logging {
  private val oneYearMillis: Long = 1000L * 60 * 60 * 24 * 365

  def cleanupAllExpiredCaches(): Seq[CacheCleanupResult] = {
    Seq(
      CacheCleanupResult("Vim", cleanupExpiredFiles(new File(new File("cache"), "Vim"), ".html", oneYearMillis))
    )
  }

  private def cleanupExpiredFiles(cacheDir: File, fileSuffix: String, retentionMillis: Long): Int = {
    val thresholdMillis = System.currentTimeMillis() - retentionMillis
    Option(cacheDir.listFiles())
      .toSeq
      .flatten
      .filter(file => file.isFile && file.getName.endsWith(fileSuffix))
      .flatMap { file =>
        readAccessedMillis(file).map(accessedMillis => (file, accessedMillis))
      }
      .filter { case (_, accessedMillis) => accessedMillis < thresholdMillis }
      .sortBy(_._2)
      .count { case (file, _) =>
        val deleted = file.delete()
        if (!deleted) {
          logger.warn(s"Failed to delete expired cache file: ${file.getAbsolutePath}")
        }
        deleted
      }
  }

  private def readAccessedMillis(file: File): Option[Long] = {
    scala.util.Try {
      val attributes: BasicFileAttributes = Files.readAttributes(file.toPath, classOf[BasicFileAttributes])
      attributes.lastAccessTime().toMillis
    }.toOption.orElse {
      scala.util.Try(file.lastModified()).toOption
    }
  }
}
