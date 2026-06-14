package services

import com.aha00a.commons.utils.StopWatch
import play.api.Logging

import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.attribute.BasicFileAttributes
import scala.jdk.CollectionConverters._

case class CacheCleanupResult(name: String, deletedCount: Int)

class CacheFileCleanupService extends Logging {
  private val oneYearMillis: Long = 1000L * 60 * 60 * 24 * 365
  private val maxDeletePerRun: Int = 1000

  def cleanupAllExpiredCaches(): Seq[CacheCleanupResult] = {
    Seq(
      CacheCleanupResult("Vim", cleanupExpiredFiles(new File(new File("cache"), "Vim"), oneYearMillis))
    )
  }

  private def cleanupExpiredFiles(cacheDir: File, retentionMillis: Long): Int = {
    StopWatch(s"CacheFileCleanupService.cleanupExpiredFiles\t${cacheDir}") {
      if (!cacheDir.isDirectory) {
        0
      } else {
        val thresholdMillis = System.currentTimeMillis() - retentionMillis
        val stream = Files.newDirectoryStream(cacheDir.toPath)
        try {
          stream.iterator().asScala
            .filter(path => Files.isRegularFile(path))
            .flatMap { path =>
              readAccessedMillis(path).map(accessedMillis => (path, accessedMillis))
            }
            .filter { case (_, accessedMillis) => accessedMillis < thresholdMillis }
            .take(maxDeletePerRun)
            .count { case (path, _) =>
              scala.util.Try(Files.deleteIfExists(path)).getOrElse {
                logger.warn(s"Failed to delete expired cache file: ${path.toAbsolutePath}")
                false
              }
            }
        } finally {
          stream.close()
        }
      }
    }
  }

  private def readAccessedMillis(path: Path): Option[Long] = {
    scala.util.Try {
      val attributes: BasicFileAttributes = Files.readAttributes(path, classOf[BasicFileAttributes])
      attributes.lastAccessTime().toMillis
    }.toOption.orElse {
      scala.util.Try(path.toFile.lastModified()).toOption
    }
  }
}
