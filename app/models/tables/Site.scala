package models.tables

import anorm.SqlParser.flatten
import anorm.SqlParser.long
import anorm.SqlParser.str
import anorm._
import zio.json._

import java.sql.Connection

case class Site(seq:Long, name:String, abbr: String, mainDomain: String) {
  def isNotFound: Boolean = this == Site.notFound
}

object Site {
  val notFound: Site = Site(-1, "notFound", "notFound", "")

  implicit val jsonDecoder: JsonDecoder[Site] = DeriveJsonDecoder.gen[Site]
  implicit val jsonEncoder: JsonEncoder[Site] = DeriveJsonEncoder.gen[Site]

  //noinspection TypeAnnotation
  def tupled = (apply _).tupled

  def select()(implicit connection: Connection): Seq[Site] = {
    SQL"""SELECT S.seq, S.name, S.abbr, S.mainDomain FROM Site S ORDER BY S.seq"""
      .as(long("seq") ~ str("name") ~ str("abbr") ~ str("mainDomain") *).map(flatten)
      .map(Site.tupled)
  }

  def updateAbbrAndMainDomain(seq: Long, abbr: String, mainDomain: String)(implicit connection: Connection): Int = {
    SQL"""
      UPDATE Site
      SET abbr = $abbr,
          mainDomain = $mainDomain,
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
