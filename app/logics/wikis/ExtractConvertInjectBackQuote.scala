package logics.wikis

import com.aha00a.commons.Implicits._
import models.WikiContext

class ExtractConvertInjectBackQuote() extends ExtractConvertInject {
  override def extract(s: String): String = {
    val regex = """`(.+?)`""".r
    regex.replaceAllIn(s, _ match {
      case regex(body) =>
        val uniqueKey = getUniqueKey
        arrayBuffer += uniqueKey -> body
        uniqueKey
      case _ => "error"
    })
  }


  override def convert(s: String)(implicit wikiContext: WikiContext): String = s"<code>${s.escapeHtml()}</code>"
}