package logics.wikis.interpreters

import models.{WikiContext, PageContent}

object InterpreterGraph {
  def apply(pageContent: PageContent)(implicit wikiContext: WikiContext): String = {
    views.html.Wiki.graph(
      pageContent.content.trim
        .split("""(\r\n|\n)+""")
        .flatMap(_.split("->").sliding(2).map(_.toArray))
      ,
      enableWikiLink = pageContent.shebang.contains("enableWikiLink")
    ).toString()
  }
}
