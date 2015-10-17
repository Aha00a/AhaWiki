package logics.wikis.interpreters

import models.PageContent

object InterpreterGraph {
  def render(pageContent: PageContent): String = {
    pageContent.extractDirective("enableWikiLink") match {
      case Some(s) =>
        views.html.Wiki.graph(pageContent.content.trim.split("""(\r\n|\n)+""").flatMap(_.split("->").sliding(2).map(_.toArray)), true).toString()
      case None =>
        views.html.Wiki.graph(pageContent.content.trim.split("""(\r\n|\n)+""").flatMap(_.split("->").sliding(2).map(_.toArray)), false).toString()
    }
  }
}
