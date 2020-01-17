package logics.wikis.interpreters

import models.{Link, PageContent, WikiContext}

object InterpreterHtml extends TraitInterpreter {
  override def interpret(content: String)(implicit wikiContext: WikiContext): String = {
    val pageContent: PageContent = PageContent(content)
    pageContent.content
  }

  override def extractLink(content: String)(implicit wikiContext: WikiContext): Seq[Link] = Seq()
}
