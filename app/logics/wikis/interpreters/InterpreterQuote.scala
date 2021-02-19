package logics.wikis.interpreters

import models.{PageContent, ContextWikiPage}

object InterpreterQuote extends TraitInterpreter {

  import models.tables.Link

  override def toHtmlString(content: String)(implicit wikiContext: ContextWikiPage): String = {
    val pageContent: PageContent = PageContent(content)
    // "AhaTracQuote")
    "<blockquote>" + InterpreterWiki.toHtmlString(pageContent.content) + "</blockquote>"
  }

  override def toSeqLink(content: String)(implicit wikiContext: ContextWikiPage): Seq[Link] = {
    val pageContent: PageContent = PageContent(content)
    InterpreterWiki.toSeqLink(pageContent.content)
  }
}
