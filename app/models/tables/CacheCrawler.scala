package models.tables

import anorm.SqlParser.long
import anorm.SqlParser.str
import anorm._
import com.aha00a.play.AnormSqlParser.localDateTime
import java.time.LocalDateTime

case class CacheCrawler(
  id: Long,
  url: String,
  dateInserted: LocalDateTime,
  dateUpdated: LocalDateTime,
  title: String,
  image: String,
  description: String,
  status: CacheCrawler.Status,
)

object CacheCrawler {
  import java.sql.Connection

  val UrlMaxLength: Int = 512
  val TitleMaxLength: Int = 256
  val ImageMaxLength: Int = 512
  val DescriptionMaxLength: Int = 512

  sealed trait Status { def value: String }
  object Status {
    case object Queued extends Status { val value: String = "Queued" }
    case object Done extends Status { val value: String = "Done" }

    def from(value: String): Status = value match {
      case Queued.value => Queued
      case Done.value => Done
      case _ => Queued
    }
  }

  val MaxAgeDays: Long = 90
  val SwrDays: Long = 90

  private val parser = long("id") ~ str("url") ~ localDateTime("dateInserted") ~ localDateTime("dateUpdated") ~
    str("title") ~ str("image") ~ str("description") ~ str("status") map {
    case id ~ url ~ dateInserted ~ dateUpdated ~ title ~ image ~ description ~ status =>
      CacheCrawler(id, url, dateInserted, dateUpdated, title, image, description, Status.from(status))
  }

  def truncate(input: String, maxLength: Int): String = input.take(maxLength)

  def selectByUrl(url: String)(implicit connection: Connection): Option[CacheCrawler] = {
    SQL"""
      SELECT id, url, dateInserted, dateUpdated, title, image, description, status
      FROM CacheCrawler
      WHERE url = $url
      LIMIT 1
    """.as(parser.singleOpt)
  }

  def upsertDone(url: String, title: String, image: String, description: String)(implicit connection: Connection): Int = {
    val savedTitle = truncate(title, TitleMaxLength)
    val savedImage = truncate(image, ImageMaxLength)
    val savedDescription = truncate(description, DescriptionMaxLength)

    SQL"""
      INSERT INTO CacheCrawler (url, title, image, description, status)
      VALUES ($url, $savedTitle, $savedImage, $savedDescription, ${Status.Done.value})
      ON DUPLICATE KEY UPDATE
        title = VALUES(title),
        image = VALUES(image),
        description = VALUES(description),
        status = ${Status.Done.value},
        dateUpdated = NOW()
    """.executeUpdate()
  }

  def isFresh(cache: CacheCrawler, now: LocalDateTime = LocalDateTime.now()): Boolean =
    cache.dateUpdated.isAfter(now.minusDays(MaxAgeDays))

  def isStaleButRevalidatable(cache: CacheCrawler, now: LocalDateTime = LocalDateTime.now()): Boolean =
    cache.dateUpdated.isAfter(now.minusDays(MaxAgeDays + SwrDays))
}
