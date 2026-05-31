package logics.wikis.interpreters

import models.{ContextWikiPage, PageContent}
import models.tables.CalculatedLink

object InterpreterGantt extends TraitInterpreter {
  override def toHtmlString(content: String)(implicit wikiContext: ContextWikiPage): String = {
    val pageContent = PageContent(content)
    val id = com.aha00a.commons.utils.UuidUtil.newString
    views.html.Wiki.InterpreterGantt(id, pageContent.content).toString()
  }

  override def toText(content: String)(implicit wikiContext: ContextWikiPage): String = {
    val pageContent = PageContent(content)
    pageContent.content
  }

  override def toSeqLink(content: String)(implicit wikiContext: ContextWikiPage): Seq[CalculatedLink] = Seq()
}
