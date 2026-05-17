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
  val TtlDays: Long = 180

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

  def selectRecent(limit: Int = 100)(implicit connection: Connection): Seq[CacheCrawler] = {
    SQL"""
      SELECT id, url, dateInserted, dateUpdated, title, image, description, status
      FROM CacheCrawler
      ORDER BY id DESC
      LIMIT $limit
    """.as(parser.*)
  }

  def selectPaged(
    page: Int = 1,
    pageSize: Int = 20,
    search: String = "",
    sortBy: String = "id",
    sortOrder: String = "desc",
  )(implicit connection: Connection): Seq[CacheCrawler] = {
    val safePage = math.max(1, page)
    val safePageSize = math.max(1, math.min(pageSize, 100))
    val offset = (safePage - 1) * safePageSize
    val normalizedSearch = s"%${search.trim}%"
    val normalizedSortBy = sortBy match {
      case "id" => "id"
      case "url" => "url"
      case "status" => "status"
      case "title" => "title"
      case "image" => "image"
      case "description" => "description"
      case "dateInserted" => "dateInserted"
      case "dateUpdated" => "dateUpdated"
      case _ => "id"
    }
    val normalizedSortOrder = if (sortOrder.equalsIgnoreCase("asc")) "ASC" else "DESC"
    val orderByClause = s"$normalizedSortBy $normalizedSortOrder, id DESC"

    SQL"""
      SELECT id, url, dateInserted, dateUpdated, title, image, description, status
      FROM CacheCrawler
      WHERE (
        $search = '' OR
        url LIKE $normalizedSearch OR
        title LIKE $normalizedSearch OR
        image LIKE $normalizedSearch OR
        description LIKE $normalizedSearch
      )
      ORDER BY #$orderByClause
      LIMIT $safePageSize OFFSET $offset
    """.as(parser.*)
  }

  def countPaged(search: String = "")(implicit connection: Connection): Long = {
    val normalizedSearch = s"%${search.trim}%"
    SQL"""
      SELECT COUNT(*)
      FROM CacheCrawler
      WHERE (
        $search = '' OR
        url LIKE $normalizedSearch OR
        title LIKE $normalizedSearch OR
        image LIKE $normalizedSearch OR
        description LIKE $normalizedSearch
      )
    """.as(SqlParser.scalar[Long].single)
  }

  def deleteByUrl(url: String)(implicit connection: Connection): Int = {
    SQL"""DELETE FROM CacheCrawler WHERE url = $url""".executeUpdate()
  }

  def deleteExpired(now: LocalDateTime = LocalDateTime.now())(implicit connection: Connection): Int = {
    val threshold = now.minusDays(TtlDays)
    SQL"""DELETE FROM CacheCrawler WHERE dateUpdated < $threshold""".executeUpdate()
  }

  def isFresh(cache: CacheCrawler, now: LocalDateTime = LocalDateTime.now()): Boolean =
    cache.dateUpdated.isAfter(now.minusDays(MaxAgeDays))

  def isStaleButRevalidatable(cache: CacheCrawler, now: LocalDateTime = LocalDateTime.now()): Boolean =
    cache.dateUpdated.isAfter(now.minusDays(MaxAgeDays + SwrDays))
}
