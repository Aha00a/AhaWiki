package logics.wikis.interpreters

import models.{ContextWikiPage, PageContent}
import models.tables.CalculatedLink

object InterpreterKanban extends TraitInterpreter {
  override def toHtmlString(content: String)(implicit wikiContext: ContextWikiPage): String = {
    val pageContent = PageContent(content)
    val id = com.aha00a.commons.utils.UuidUtil.newString
    val shebangLine = pageContent.shebang.mkString(" ")
    views.html.Wiki.InterpreterKanban(id, pageContent.content, shebangLine).toString()
  }

  override def toText(content: String)(implicit wikiContext: ContextWikiPage): String = {
    val pageContent = PageContent(content)
    InterpreterWiki.toText(pageContent.content)
  }

  override def toSeqLink(content: String)(implicit wikiContext: ContextWikiPage): Seq[CalculatedLink] = {
    val pageContent = PageContent(content)
    InterpreterWiki.toSeqLink(pageContent.content)
  }
}
