package logics.wikis.interpreters

import models.{PageContent, WikiContext}

object InterpreterGraph {
  def apply(pageContent: PageContent)(implicit wikiContext: WikiContext): String = {
    val lines = pageContent.content.trim.split("""(\r\n|\n)+""")
    val linesCut = if(wikiContext.isPreview) {lines.take(100)} else {lines}
    views.html.Wiki.graph(linesCut.flatMap(_.split("->").sliding(2).map(_.toArray)), enableWikiLink = pageContent.shebang.contains("enableWikiLink")).toString()
  }
}
