package logics.wikis.interpreters
import models.{Link, PageContent, WikiContext}

object InterpreterMarkdown extends TraitInterpreter {
  override def interpret(content: String)(implicit wikiContext: WikiContext): String = {
    val pageContent: PageContent = PageContent(content)
    com.github.rjeschke.txtmark.Processor.process(pageContent.content)
  }

  override def extractLink(content: String)(implicit wikiContext: WikiContext): Seq[Link] = {
    // TODO: implement
    Seq()
  }
}
