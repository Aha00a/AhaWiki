package logics.wikis

import implicits.Implicits._
import models.WikiContext

class ExtractConvertApplyBackQuote() extends ExtractConvertApply {
  def extract(s: String):String = {
    val regex = """`(.+?)`""".r
    regex.replaceAllIn(s, _ match {
      case regex(body) =>
        val uniqueKey = getUniqueKey
        map.put(uniqueKey, body)
        uniqueKey
      case _ => "error"
    })
  }


  def convert(s:String)(implicit wikiContext:WikiContext):String = s"<code>${s.escapeHtml()}</code>"
}
