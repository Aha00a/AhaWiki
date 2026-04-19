package logics.wikis

import com.aha00a.commons.utils.ShebangUtil
import logics.wikis.interpreters.Interpreters
import models.ContextWikiPage

class ExtractConvertInjectInterpreter() extends ExtractConvertInject {

  import models.tables.CalculatedLink
  import models.tables.CalculatedSchemaOrg

  override def extract(s: String): String = {
    extractByMarkers(s)
  }

  override def convert(s: String)(implicit wikiContext: ContextWikiPage): String = Interpreters.toHtmlString(ShebangUtil.addWhenNotExist(s, "text"))

  def extractLink()(implicit wikiContext: ContextWikiPage): Seq[CalculatedLink] = {
    arrayBuffer.map(_._2).flatMap(c => Interpreters.toSeqLink(c)).toSeq
  }

  def extractSchemaOrg()(implicit wikiContext: ContextWikiPage): Seq[CalculatedSchemaOrg] = {
    arrayBuffer.map(_._2).flatMap(c => Interpreters.toSeqSchemaOrg(c)).toSeq
  }
}
