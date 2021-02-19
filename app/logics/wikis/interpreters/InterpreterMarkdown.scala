package logics.wikis.interpreters
import models.{PageContent, ContextWikiPage}

object InterpreterMarkdown extends TraitInterpreter {

  import models.tables.Link

  override def toHtmlString(content: String)(implicit wikiContext: ContextWikiPage): String = {
    val pageContent: PageContent = PageContent(content)
    com.github.rjeschke.txtmark.Processor.process(pageContent.content)
  }

  override def toSeqLink(content: String)(implicit wikiContext: ContextWikiPage): Seq[Link] = {
    // TODO: implement
    Seq()
  }
}
