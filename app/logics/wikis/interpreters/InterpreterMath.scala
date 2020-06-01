package logics.wikis.interpreters
import models.{Link, PageContent, WikiContext}

object InterpreterMath extends TraitInterpreter {
  override def toHtmlString(content: String)(implicit wikiContext: WikiContext): String = {
    val pageContent: PageContent = PageContent(content)
    """<span class="mathjax">$___$""" + pageContent.content + """$___$</span>"""
  }

  override def toSeqLink(content: String)(implicit wikiContext: WikiContext): Seq[Link] = Seq()
}
