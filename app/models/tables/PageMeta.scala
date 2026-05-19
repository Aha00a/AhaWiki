package models.tables

import anorm.SqlParser.{flatten, long, str}
import anorm._
import com.aha00a.play.AnormSqlParser.localDateTime
import models.PageLatestSummary

import java.sql.Connection
import java.time.LocalDateTime

case class PageMeta(
  name: String,
  revision: Long,
  dateInserted: LocalDateTime,
  dateUpdated: Option[LocalDateTime],
  image: Option[String],
  size: Long,
)

case class AdminPageMetaRow(name: String, revision: Long, dateUpdated: Option[LocalDateTime], image: Option[String], size: Long)

object PageMeta {
  private val ImageMaxLength = 512

  private def truncate(value: String, maxLength: Int): String = {
    if (value.length <= maxLength) value else value.take(maxLength)
  }

  private val rowParser = str("name") ~ long("revision") ~ localDateTime("dateInserted") ~ localDateTime("dateUpdated").? ~ str("image").? ~ long("size")
  private val rowParserPageLatestSummary = str("name") ~ long("revision") ~ localDateTime("dateTime") ~ long("user").? ~ str("image").? ~ long("size")

  def upsert(
    pageName: String,
    revision: Long,
    image: Option[String],
    size: Long,
    dateUpdated: LocalDateTime = LocalDateTime.now(),
  )(implicit connection: Connection, site: Site): Int = {
    val savedImage = image.map(img => truncate(img, ImageMaxLength))
    SQL"""
      INSERT INTO PageMeta (site, name, revision, dateUpdated, image, size)
      VALUES (${site.seq}, $pageName, $revision, $dateUpdated, $savedImage, $size)
      ON DUPLICATE KEY UPDATE
        revision = VALUES(revision),
        dateUpdated = VALUES(dateUpdated),
        image = VALUES(image),
        size = VALUES(size)
    """.executeUpdate()
  }

  def delete(name: String)(implicit connection: Connection, site: Site): Int = {
    SQL"DELETE FROM PageMeta WHERE site = ${site.seq} AND name = $name".executeUpdate()
  }

  //noinspection TypeAnnotation
  def tupled = (apply _).tupled


  def selectMissingPageNames(limit: Int = 1)(implicit connection: Connection, site: Site): List[String] = {
    SQL"""
      SELECT p.name
      FROM (
        SELECT DISTINCT name
        FROM Page
        WHERE site = ${site.seq}
      ) p
      LEFT JOIN PageMeta pm ON pm.site = ${site.seq} AND pm.name = p.name
      WHERE pm.name IS NULL
      ORDER BY RAND()
      LIMIT $limit
    """.as(SqlParser.str("name").*)
  }


  def selectSeqName()(implicit connection: Connection, site: Site): Seq[String] = {
    SQL"""
      SELECT name
      FROM PageMeta
      WHERE site = ${site.seq}
      ORDER BY name DESC
    """.as(SqlParser.str("name").*)
  }
  def selectSeqPageLatestSummary()(implicit connection: Connection, site: Site): Seq[PageLatestSummary] = {
    SQL"""
      SELECT PM.name, PM.revision, P.dateTime, P.user, PM.image, PM.size
      FROM PageMeta PM
      INNER JOIN Page P
        ON P.site = PM.site
        AND P.name = PM.name
        AND P.revision = PM.revision
      WHERE PM.site = ${site.seq}
      ORDER BY PM.name DESC
    """.as(rowParserPageLatestSummary.*).map(flatten).map(PageLatestSummary.tupled)
  }


  private val adminPageMetaRowParser = str("name") ~ long("revision") ~ localDateTime("dateUpdated").? ~ str("image").? ~ long("size") map {
    case name ~ revision ~ dateUpdated ~ image ~ size =>
      AdminPageMetaRow(name, revision, dateUpdated, image, size)
  }

  def selectPagedForAdmin(page: Int, pageSize: Int, search: String, sortBy: String, sortOrder: String)(implicit connection: Connection, site: Site): Seq[AdminPageMetaRow] = {
    val normalizedSortBy = sortBy match {
      case "name" => "name"
      case "revision" => "revision"
      case "image" => "image"
      case "dateUpdated" => "dateUpdated"
      case "size" => "size"
      case _ => "dateUpdated"
    }
    val normalizedSortOrder = if (sortOrder.equalsIgnoreCase("asc")) "ASC" else "DESC"
    val offset = (page - 1).max(0) * pageSize
    val searchLike = s"%${search.trim}%"
    SQL(s"""
      SELECT name, revision, dateUpdated, image, size
      FROM PageMeta
      WHERE site = {siteSeq}
        AND ({search} = '' OR name LIKE {searchLike} OR IFNULL(image, '') LIKE {searchLike})
      ORDER BY $normalizedSortBy $normalizedSortOrder
      LIMIT {pageSize} OFFSET {offset}
    """).on(
      "siteSeq" -> site.seq,
      "search" -> search,
      "searchLike" -> searchLike,
      "pageSize" -> pageSize,
      "offset" -> offset,
    ).as(adminPageMetaRowParser.*)
  }

  def countPagedForAdmin(search: String)(implicit connection: Connection, site: Site): Long = {
    val searchLike = s"%${search.trim}%"
    SQL"""
      SELECT COUNT(*) count_value
      FROM PageMeta
      WHERE site = ${site.seq}
        AND ($search = '' OR name LIKE $searchLike OR IFNULL(image, '') LIKE $searchLike)
    """.as(long("count_value").single)
  }

  def select(name: String)(implicit connection: Connection, site: Site): Option[PageMeta] = {
    SQL"""
      SELECT name, revision, dateInserted, dateUpdated, image, size
      FROM PageMeta
      WHERE site = ${site.seq} AND name = $name
    """.as(rowParser.singleOpt).map(flatten).map(PageMeta.tupled)
  }

  def selectImageMap(names: Seq[String])(implicit connection: Connection, site: Site): Map[String, String] = {
    val distinctNames = names.distinct
    if (distinctNames.isEmpty) {
      Map.empty
    } else {
      SQL"""
        SELECT name, image
        FROM PageMeta
        WHERE site = ${site.seq}
          AND name IN ($distinctNames)
          AND image IS NOT NULL
          AND image <> ''
      """.as((str("name") ~ str("image")).map { case name ~ image => name -> image }.*).toMap
    }
  }
}
