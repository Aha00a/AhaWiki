package models

import org.scalatest.freespec.AnyFreeSpec
import zio.json._

import java.time.Instant
import java.time.LocalDateTime
import java.util.Date
import JsonEncoderDecoderForDate._
import com.aha00a.commons.Implicits.RichDate

import java.text.SimpleDateFormat

case class Banana(curvature: Double, instant: Instant, date: Date, localDateTime: LocalDateTime)

object Banana {
  implicit val decoder: JsonDecoder[Banana] = DeriveJsonDecoder.gen[Banana]
  implicit val encoder: JsonEncoder[Banana] = DeriveJsonEncoder.gen[Banana]
}

object A {
  def aa[T](s: String)(implicit decoder: JsonDecoder[T]): Either[String, T] = {
    s.fromJson[T]
  }

  def toJson[T](t: T)(implicit encoder: JsonEncoder[T]): String = {
    t.toJson
  }
}

class JsonSpec extends AnyFreeSpec {
  "parse" in {
    val res: Either[String, Banana] = A.aa[Banana]("""{"curvature":0.5, "date":"2015-11-26T14:26:09.729+09:00", "instant": "2015-11-26T14:26:09.729Z", "localDateTime": "2015-11-26T14:26:09.729"}""")
    res match {
      case Left(e) => println(s"Error: $e")
      case Right(banana) => println(s"Parsed: $banana")
    }
    assert(res.isRight)

    val date = new SimpleDateFormat("yyyy-MM-dd").parse("2025-05-16")
    assert(A.toJson(Banana(0.5, date.toInstant, date, date.toLocalDateTime)) ===
      """{"curvature":0.5,"instant":"2025-05-15T15:00:00Z","date":"2025-05-16T00:00:00.000+09:00","localDateTime":"2025-05-16T00:00:00"}""")
  }
}
