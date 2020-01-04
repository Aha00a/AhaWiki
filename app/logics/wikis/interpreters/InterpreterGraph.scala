package logics.wikis.interpreters

import logics.wikis.RenderingMode
import models.{PageContent, WikiContext}

object InterpreterGraph {
  def apply(pageContent: PageContent)(implicit wikiContext: WikiContext): String = {
    val lines = pageContent.content.trim.split("""(\r\n|\n)+""")
    val linesCut = wikiContext.renderingMode match {
      case RenderingMode.Normal => lines
      case RenderingMode.Preview => lines.take(100)
    }
    views.html.Wiki.graph(linesCut.flatMap(_.split("->").sliding(2).map(_.toArray)), enableWikiLink = pageContent.shebang.contains("enableWikiLink")).toString()
  }
}
