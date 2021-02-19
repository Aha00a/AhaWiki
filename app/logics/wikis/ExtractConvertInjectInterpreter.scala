package logics.wikis

import com.aha00a.commons.utils.ShebangUtil
import logics.wikis.interpreters.Interpreters
import models.ContextWikiPage

class ExtractConvertInjectInterpreter() extends ExtractConvertInject {

  import models.tables.Link
  import models.tables.SchemaOrg

  override def extract(s: String): String = {
    if (s == null || !s.contains("[[[") || !s.contains("]]]")) {
      s
    } else {
      val Array(head, remain) = s.split("""\[\[\[""", 2)
      val Array(body, tail) = remain.split("""\]\]\]""", 2)
      val uniqueKey = getUniqueKey
      arrayBuffer += uniqueKey -> body
      head + uniqueKey + extract(tail)
    }
  }

  override def convert(s: String)(implicit wikiContext: ContextWikiPage): String = Interpreters.toHtmlString(ShebangUtil.addWhenNotExist(s, "text"))

  def extractLink()(implicit wikiContext: ContextWikiPage): Seq[Link] = {
    arrayBuffer.map(_._2).flatMap(c => Interpreters.toSeqLink(c)).toSeq
  }

  def extractSchemaOrg()(implicit wikiContext: ContextWikiPage): Seq[SchemaOrg] = {
    arrayBuffer.map(_._2).flatMap(c => Interpreters.toSeqSchemaOrg(c)).toSeq
  }
}

