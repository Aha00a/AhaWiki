package logics.wikis.interpreters
import models.{PageContent, ContextWikiPage}

object InterpreterMath extends TraitInterpreter {

  import models.tables.Link

  override def toHtmlString(content: String)(implicit wikiContext: ContextWikiPage): String = {
    val pageContent: PageContent = PageContent(content)
    """<span class="mathjax">$___$""" + pageContent.content + """$___$</span>"""
  }

  override def toSeqLink(content: String)(implicit wikiContext: ContextWikiPage): Seq[Link] = Seq()
}
