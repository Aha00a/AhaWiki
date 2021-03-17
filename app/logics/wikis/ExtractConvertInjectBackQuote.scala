package logics.wikis

import com.aha00a.commons.Implicits._
import models.WikiContext

class ExtractConvertInjectBackQuote() extends ExtractConvertInject {
  override def extract(s: String): String = {
    val regexDoubleBackquote = """``(.+?)``""".r
    val s1 = regexDoubleBackquote.replaceAllIn(s, _ match {
      case regexDoubleBackquote(body) =>
        val uniqueKey = getUniqueKey
        arrayBuffer += uniqueKey -> <input value={body} class="MacroCopyable" readonly="readonly"/>.toString
        uniqueKey
      case _ => "error"
    })
    val regexSingleBackquote = """`(.+?)`""".r
    regexSingleBackquote.replaceAllIn(s1, _ match {
      case regexSingleBackquote(body) =>
        val uniqueKey = getUniqueKey
        arrayBuffer += uniqueKey -> s"<code>${body.escapeHtml()}</code>"
        uniqueKey
      case _ => "error"
    })
  }


  override def convert(s: String)(implicit wikiContext: WikiContext): String = s
}
