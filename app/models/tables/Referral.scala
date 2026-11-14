package models.tables

import anorm.SqlParser.{get, long, str}
import anorm._

import java.net.URI
import java.sql.Connection
import java.time.LocalDate
import scala.util.Try

/**
 * Where readers come from: per site and day, how many times a page on another host linked a
 * reader to a page here (evolution 73).
 *
 * A count, not a log. Nothing names the reader; the source is the linking page's host and path
 * and the landing is the path here, both without their query strings, which is where search
 * terms, tokens and tracking ids travel.
 */
object Referral {

  /** The longest source or landing kept; the columns are VARCHAR(255). */
  val MaxLength = 255

  case class Visit(source: String, landing: String)

  /**
   * The referral a request makes, if it is one worth counting: a page read (GET, 200, the root
   * or a /w/ page) that a page on another host linked to. Everything else -- assets, the API,
   * redirects, errors, a reader moving between pages of the same site, a Referer that is not an
   * http(s) address -- is not a referral.
   */
  def of(method: String, status: Int, uri: String, host: String, referer: Option[String]): Option[Visit] = {
    val landingPath = uri.takeWhile(c => c != '?' && c != '#')
    val isPageRead = method == "GET" && status == 200 && (landingPath == "/" || landingPath.startsWith("/w/"))
    if (!isPageRead) None
    else for {
      raw <- referer.map(_.trim).filter(_.nonEmpty)
      from <- Try(new URI(raw)).toOption
      scheme <- Option(from.getScheme).map(_.toLowerCase) if scheme == "http" || scheme == "https"
      fromHost <- Option(from.getHost).map(_.toLowerCase) if fromHost.nonEmpty
      if fromHost != hostWithoutPort(host)
    } yield {
      val fromPath = Option(from.getPath).filter(_.nonEmpty).getOrElse("/")
      Visit(cut(fromHost + fromPath), cut(decodePath(landingPath)))
    }
  }

  private def hostWithoutPort(host: String): String = host.toLowerCase.takeWhile(_ != ':')

  /** `/w/%EC%98%88` as `/w/예`, so the admin reads page names; left as it is if it will not parse. */
  private def decodePath(rawPath: String): String =
    Try(new URI(rawPath).getPath).toOption.filter(_ != null).getOrElse(rawPath)

  private def cut(s: String): String = if (s.length <= MaxLength) s else s.take(MaxLength)

  /** One more visit on that day. */
  def record(site: Long, day: LocalDate, visit: Visit)(implicit connection: Connection): Int =
    SQL"""
      INSERT INTO Referral (site, `day`, source, landing, `count`)
      VALUES ($site, $day, ${visit.source}, ${visit.landing}, 1)
      ON DUPLICATE KEY UPDATE `count` = `count` + 1, lastSeen = CURRENT_TIMESTAMP
    """.executeUpdate()

  case class Row(site: Long, siteName: String, source: String, landing: String, count: Int)

  /** The referrals since `from`, most counted first, at most `limit` of them. */
  def since(from: LocalDate, limit: Int)(implicit connection: Connection): Seq[Row] =
    SQL"""
      SELECT r.site, s.name, r.source, r.landing, SUM(r.`count`) AS total
        FROM Referral r
        JOIN Site s ON s.seq = r.site
       WHERE r.`day` >= $from
       GROUP BY r.site, s.name, r.source, r.landing
       ORDER BY total DESC, r.source, r.landing
       LIMIT $limit
    """.as((long("site") ~ str("name") ~ str("source") ~ str("landing") ~ get[BigDecimal]("total")).map {
      // SUM is DECIMAL on MySQL and BIGINT on H2; BigDecimal reads both.
      case site ~ name ~ source ~ landing ~ total => Row(site, name, source, landing, total.toInt)
    }.*)
}
