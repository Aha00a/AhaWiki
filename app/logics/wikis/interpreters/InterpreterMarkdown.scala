package logics.wikis.interpreters
import models.{PageContent, WikiContext}

object InterpreterMarkdown extends TraitInterpreter {

  import models.tables.Link

  override def toHtmlString(content: String)(implicit wikiContext: WikiContext): String = {
    val pageContent: PageContent = PageContent(content)
    com.github.rjeschke.txtmark.Processor.process(pageContent.content)
  }

  override def toSeqLink(content: String)(implicit wikiContext: WikiContext): Seq[Link] = {
    // TODO: implement
    Seq()
  }
}
