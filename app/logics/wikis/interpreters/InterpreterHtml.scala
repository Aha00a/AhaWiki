package logics.wikis.interpreters

import models.{PageContent, ContextWikiPage}
import org.jsoup.Jsoup

object InterpreterHtml extends TraitInterpreter {

  import models.tables.Link

  override def toHtmlString(content: String)(implicit wikiContext: ContextWikiPage): String = {
    val pageContent: PageContent = PageContent(content)
    pageContent.content
  }

  override def toSeqLink(content: String)(implicit wikiContext: ContextWikiPage): Seq[Link] = Seq()
}
