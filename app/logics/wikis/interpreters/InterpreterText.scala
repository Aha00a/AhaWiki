package logics.wikis.interpreters

import models.{PageContent, WikiContext}

object InterpreterText extends TraitInterpreter {

  import models.tables.Link

  override def toHtmlString(content: String)(implicit wikiContext: WikiContext): String = {
    val pageContent: PageContent = PageContent(content)
    val contentEscaped = pageContent.content.replaceAll("""&""", "&amp;").replaceAll("<", "&lt;")
    s"""<pre class="text">$contentEscaped</pre>"""
  }

  override def toSeqLink(content: String)(implicit wikiContext: WikiContext): Seq[Link] = Seq()
}
