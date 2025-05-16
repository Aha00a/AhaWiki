package models

import zio.json._

import java.text.SimpleDateFormat
import java.util.Date

object JsonEncoderDecoderForDate {
  implicit val dateEncoder: JsonEncoder[Date] = JsonEncoder.string.contramap[Date] { date =>
    val sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSXXX")
    sdf.format(date)
  }

  implicit val dateDecoder: JsonDecoder[Date] = JsonDecoder.string.map { str =>
    val sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSXXX")
    sdf.parse(str)
  }
}
