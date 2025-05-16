package models.tables

import anorm.SqlParser.flatten
import anorm.SqlParser.long
import anorm.SqlParser.str
import anorm._
import com.aha00a.play.AnormSqlParser.localDateTime
import zio.json._

import java.sql.Connection
import java.time.LocalDateTime


case class SiteDomain(created: LocalDateTime, site: Long, domain: String)

object SiteDomain {
  val notFound: SiteDomain = SiteDomain(null, -1, "notFound")

  import models.JsonEncoderDecoderForDate._
  implicit val jsonDecoder: JsonDecoder[SiteDomain] = DeriveJsonDecoder.gen[SiteDomain]
  implicit val jsonEncoder: JsonEncoder[SiteDomain] = DeriveJsonEncoder.gen[SiteDomain]

  //noinspection TypeAnnotation
  def tupled = (apply _).tupled

  def select()(implicit connection: Connection): Seq[SiteDomain] = {
    SQL"""SELECT * FROM SiteDomain ORDER BY site"""
      .as(localDateTime("created") ~ long("site") ~ str("domain") *).map(flatten)
      .map(SiteDomain.tupled)
  }
}
