package logics.wikis

import com.aha00a.commons.utils.ShebangUtil
import logics.wikis.interpreters.Interpreters
import models.{Link, SchemaOrg, WikiContext}

class ExtractConvertApplyInterpreter() extends ExtractConvertApply {

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

  override def convert(s: String)(implicit wikiContext: WikiContext): String = Interpreters.interpret(ShebangUtil.addWhenNotExist(s, "text"))

  def extractLink()(implicit wikiContext: WikiContext): Seq[Link] = {
    arrayBuffer.map(_._2).flatMap(c => Interpreters.extractLink(c))
  }

  def extractSchemaOrg()(implicit wikiContext: WikiContext): Seq[SchemaOrg] = {
    arrayBuffer.map(_._2).flatMap(c => Interpreters.extractSchema(c))
  }
}

