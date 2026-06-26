package models

import zio.json._

import java.time.LocalDateTime

case class PageLatestSummary(
  name: String,
  revision: Long,
  dateTime: LocalDateTime,
  user: Option[Long],
  image: Option[String],
  description: Option[String],
  size: Long,
) extends WithDateTime

object PageLatestSummary {
  import models.JsonEncoderDecoderForDate._

  implicit val jsonDecoder: JsonDecoder[PageLatestSummary] = DeriveJsonDecoder.gen[PageLatestSummary]
  implicit val jsonEncoder: JsonEncoder[PageLatestSummary] = DeriveJsonEncoder.gen[PageLatestSummary]

  //noinspection TypeAnnotation
  def tupled = (apply _).tupled
}
