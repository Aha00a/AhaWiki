package logics.wikis.interpreters

import models.{Link, PageContent, WikiContext}

object InterpreterQuote extends TraitInterpreter {
  override def toHtmlString(content: String)(implicit wikiContext: WikiContext): String = {
    val pageContent: PageContent = PageContent(content)
    // "AhaTracQuote")
    "<blockquote>" + InterpreterWiki.toHtmlString(pageContent.content) + "</blockquote>"
  }

  override def toSeqLink(content: String)(implicit wikiContext: WikiContext): Seq[Link] = {
    val pageContent: PageContent = PageContent(content)
    InterpreterWiki.toSeqLink(pageContent.content)
  }
}
