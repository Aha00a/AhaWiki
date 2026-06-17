package com.aha00a.tests.unit

import com.aha00a.tests.TestUtil
import models.JsonEncoderDecoderForDate._
import com.aha00a.commons.Implicits.RichDate
import zio.json._

import java.text.SimpleDateFormat
import java.time.Instant
import java.time.LocalDateTime
import java.util.Date

object JsonUnit {
  case class Banana(curvature: Double, instant: Instant, date: Date, localDateTime: LocalDateTime)

  object Banana {
    implicit val decoder: JsonDecoder[Banana] = DeriveJsonDecoder.gen[Banana]
    implicit val encoder: JsonEncoder[Banana] = DeriveJsonEncoder.gen[Banana]
  }

  object JsonCodec {
    def parse[T](s: String)(implicit decoder: JsonDecoder[T]): Either[String, T] = s.fromJson[T]
    def toJson[T](t: T)(implicit encoder: JsonEncoder[T]): String = t.toJson
  }

  def run(testUtil: TestUtil): Unit = {
    import testUtil.assertEquals

    val res: Either[String, Banana] = JsonCodec.parse[Banana]("""{"curvature":0.5, "date":"2015-11-26T14:26:09.729+09:00", "instant": "2015-11-26T14:26:09.729Z", "localDateTime": "2015-11-26T14:26:09.729"}""")
    assertEquals(res.isRight, true)

    val date = new SimpleDateFormat("yyyy-MM-dd").parse("2025-05-16")
    assertEquals(
      JsonCodec.toJson(Banana(0.5, date.toInstant, date, date.toLocalDateTime)),
      """{"curvature":0.5,"instant":"2025-05-15T15:00:00Z","date":"2025-05-16T00:00:00.000+09:00","localDateTime":"2025-05-16T00:00:00"}"""
    )
  }
}
