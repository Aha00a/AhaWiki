package logics.wikis.interpreters

import models.ContextWikiPage
import models.PageContent

object InterpreterMermaid extends TraitInterpreter {

  import models.tables.Link

  override def toHtmlString(content: String)(implicit wikiContext: ContextWikiPage): String = {
    val pageContent: PageContent = PageContent(content)
    """<div class="mermaid">""" + pageContent.content + """</div>"""
  }

  override def toSeqLink(content: String)(implicit wikiContext: ContextWikiPage): Seq[Link] = Seq()
}
