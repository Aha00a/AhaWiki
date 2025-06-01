package logics.wikis.interpreters

import models.{PageContent, ContextWikiPage}

object InterpreterText extends TraitInterpreter {

  import models.tables.CalculatedLink

  override def toHtmlString(content: String)(implicit wikiContext: ContextWikiPage): String = {
    val pageContent: PageContent = PageContent(content)
    val contentEscaped = pageContent.content.replaceAll("""&""", "&amp;").replaceAll("<", "&lt;")
    s"""<pre class="text">$contentEscaped</pre>"""
  }

  override def toSeqLink(content: String)(implicit wikiContext: ContextWikiPage): Seq[CalculatedLink] = Seq()
}
