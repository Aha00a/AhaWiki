package logics.wikis.interpreters
import models.{PageContent, ContextWikiPage}

object InterpreterMarkdown extends TraitInterpreter {

  import models.tables.CalculatedLink

  override def toHtmlString(content: String)(implicit wikiContext: ContextWikiPage): String = {
    val pageContent: PageContent = PageContent(content)
    com.github.rjeschke.txtmark.Processor.process(pageContent.content)
  }

  override def toSeqLink(content: String)(implicit wikiContext: ContextWikiPage): Seq[CalculatedLink] = {
    // TODO: implement
    Seq()
  }
}
