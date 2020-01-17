package logics.wikis.interpreters

import models.{Link, PageContent, WikiContext}

object InterpreterQuote extends TraitInterpreter {
  override def interpret(content: String)(implicit wikiContext: WikiContext): String = {
    val pageContent: PageContent = PageContent(content)
    // "AhaTracQuote")
    "<blockquote>" + InterpreterWiki.interpret(pageContent.content) + "</blockquote>"
  }

  override def extractLink(content: String)(implicit wikiContext: WikiContext): Seq[Link] = {
    val pageContent: PageContent = PageContent(content)
    InterpreterWiki.extractLink(pageContent.content)
  }
}
