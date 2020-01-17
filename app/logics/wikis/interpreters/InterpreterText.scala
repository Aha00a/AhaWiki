package logics.wikis.interpreters

import models.{PageContent, WikiContext}

object InterpreterText extends TraitInterpreter {
  override def interpret(content: String)(implicit wikiContext: WikiContext): String = {
    val pageContent: PageContent = PageContent(content)
    val contentEscaped = pageContent.content.replaceAll("""&""", "&amp;").replaceAll("<", "&lt;")
    s"""<pre class="text">$contentEscaped</pre>"""
  }
}
