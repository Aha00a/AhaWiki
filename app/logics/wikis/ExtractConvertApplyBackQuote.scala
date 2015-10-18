package logics.wikis

import models.WikiContext
import utils.UuidUtil
import implicits.Implicits._

class ExtractConvertApplyBackQuote() extends ExtractConvertApply {
  def extract(s: String):String = {
    val regex = """`(.+?)`""".r
    regex.replaceAllIn(s, _ match {
      case regex(body) =>
        val uuid = UuidUtil.newString
        map.put(uuid, body)
        uuid
      case _ => "error"
    })
  }

  def convert(s:String)(implicit wikiContext:WikiContext):String = s"<code>${s.escapeHtml()}</code>"
}
