package logics.wikis

import models.WikiContext

// TODO: rename to ExtractConvertInject
// TODO: Refactor this.
class ExtractConvertApplyInterpreterCustom(converter:String => String) extends ExtractConvertApply {
  override def extract(s: String): String = {
    if (s == null || !s.contains("[[[") || !s.contains("]]]")) {
      s
    } else {
      val Array(head, remain) = s.split( """\[\[\[""", 2)
      val Array(body, tail) = remain.split( """\]\]\]""", 2)
      val uniqueKey = getUniqueKey
      arrayBuffer += uniqueKey -> body
      head + uniqueKey + extract(tail)
    }
  }

  override def convert(s: String)(implicit wikiContext: WikiContext): String = converter(s)
}

