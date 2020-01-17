package logics.wikis.interpreters
import models.{PageContent, WikiContext}

object InterpreterMath extends TraitInterpreter {
  override def interpret(content: String)(implicit wikiContext: WikiContext): String = {
    val pageContent: PageContent = PageContent(content)
    """<span class="mathjax">$___$""" + pageContent.content + """$___$</span>"""
  }
}
