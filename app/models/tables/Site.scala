package models.tables

import anorm.SqlParser.get
import anorm.SqlParser.long
import anorm.SqlParser.str
import anorm._
import zio.json._

import java.sql.Connection

case class Site(seq: Long, name: String, abbr: String, mainDomain: String, publicListedOrder: Option[BigDecimal] = None) {
  def isNotFound: Boolean = this == Site.notFound
}

object Site {
  val notFound: Site = Site(-1, "notFound", "notFound", "")

  implicit val jsonDecoder: JsonDecoder[Site] = DeriveJsonDecoder.gen[Site]
  implicit val jsonEncoder: JsonEncoder[Site] = DeriveJsonEncoder.gen[Site]

  //noinspection TypeAnnotation
  def tupled = (apply _).tupled

  private val parser =
    (long("seq") ~ str("name") ~ str("abbr") ~ str("mainDomain") ~ get[Option[BigDecimal]]("publicListedOrder")).map {
      case seq ~ name ~ abbr ~ mainDomain ~ publicListedOrder =>
        Site(seq, name, abbr, mainDomain, publicListedOrder)
    }

  def select()(implicit connection: Connection): Seq[Site] = {
    SQL"""SELECT S.seq, S.name, S.abbr, S.mainDomain, S.publicListedOrder FROM Site S ORDER BY S.seq"""
      .as(parser.*)
  }

  def selectPublicListed()(implicit connection: Connection): Seq[Site] = {
    SQL"""
      SELECT S.seq, S.name, S.abbr, S.mainDomain, S.publicListedOrder
      FROM Site S
      WHERE S.publicListedOrder IS NOT NULL
        AND S.publicListedOrder > 0
        AND S.mainDomain != ''
      ORDER BY S.publicListedOrder DESC, S.seq ASC
    """.as(parser.*)
  }

  def updateAbbrAndMainDomain(
    seq: Long,
    abbr: String,
    mainDomain: String,
    publicListedOrder: Option[BigDecimal],
  )(implicit connection: Connection): Int = {
    SQL"""
      UPDATE Site
      SET abbr = $abbr,
          mainDomain = $mainDomain,
          publicListedOrder = $publicListedOrder,
          updated = NOW()
      WHERE seq = $seq
    """.executeUpdate()
  }

//  def selectWhereDomain(domain: String)(implicit connection: Connection): Option[Site] = {
//    SQL"""
//        SELECT
//            S.seq, S.name
//            FROM Site S
//            INNER JOIN SiteDomain SD ON S.seq = SD.site
//            WHERE SD.domain = $domain
//         """
//      .as(long("seq") ~ str("name") singleOpt).map(flatten)
//      .map(Site.tupled)
//  }

}
