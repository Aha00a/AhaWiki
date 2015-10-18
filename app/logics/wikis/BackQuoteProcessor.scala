package logics.wikis

import models.WikiContext
import utils.UuidUtil
import implicits.Implicits._

class BackQuoteProcessor() extends ExtractApply {
  def extract(s: String)(implicit wikiContext:WikiContext):String = {
    val regex = """`(.+?)`""".r
    regex.replaceAllIn(s, _ match {
      case regex(body) =>
        val uuid = UuidUtil.newString
        map.put(uuid, s"<code>${body.escapeHtml()}</code>")
        uuid
      case _ => "error"
    })
  }
}
