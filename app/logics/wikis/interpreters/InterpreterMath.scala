package logics.wikis.interpreters
import models.{PageContent, WikiContext}

object InterpreterMath extends TraitInterpreter {

  import models.tables.Link

  override def toHtmlString(content: String)(implicit wikiContext: WikiContext): String = {
    val pageContent: PageContent = PageContent(content)
    """<span class="mathjax">$___$""" + pageContent.content + """$___$</span>"""
  }

  override def toSeqLink(content: String)(implicit wikiContext: WikiContext): Seq[Link] = Seq()
}
