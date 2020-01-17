package logics.wikis.interpreters
import models.{PageContent, WikiContext}

object InterpreterHtml extends TraitInterpreter {
  override def interpret(content: String)(implicit wikiContext: WikiContext): String = {
    val pageContent: PageContent = PageContent(content)
    pageContent.content
  }
}
