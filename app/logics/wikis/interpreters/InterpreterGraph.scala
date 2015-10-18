package logics.wikis.interpreters

import models.{WikiContext, PageContent}

object InterpreterGraph {
  def interpret(pageContent: PageContent)(implicit wikiContext: WikiContext): String = {
    pageContent.extractDirective("enableWikiLink") match {
      case Some(s) =>
        views.html.Wiki.graph(pageContent.content.trim.split("""(\r\n|\n)+""").flatMap(_.split("->").sliding(2).map(_.toArray)), true).toString()
      case None =>
        views.html.Wiki.graph(pageContent.content.trim.split("""(\r\n|\n)+""").flatMap(_.split("->").sliding(2).map(_.toArray)), false).toString()
    }
  }
}
