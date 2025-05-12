package models.tables

import anorm.SqlParser.flatten
import anorm.SqlParser.long
import anorm.SqlParser.str
import anorm._

import java.sql.Connection

case class Site(seq:Long, name:String) {
  def isNotFound: Boolean = this == Site.notFound
}

object Site {
  val notFound: Site = Site(-1, "notFound")

  //noinspection TypeAnnotation
  def tupled = (apply _).tupled

  def select()(implicit connection: Connection): Seq[Site] = {
    SQL"""SELECT S.seq, S.name FROM Site S ORDER BY S.seq"""
      .as(long("seq") ~ str("name") *).map(flatten)
      .map(Site.tupled)
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
