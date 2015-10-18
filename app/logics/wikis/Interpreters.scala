package logics.wikis

import logics.wikis.interpreters._
import models.{PageContent, WikiContext}
import play.api.Logger

object Interpreters {
  def interpret(s: String)(implicit wikiContext:WikiContext): String = {
    val pageContent: PageContent = new PageContent(s)
    val body = pageContent.content
    val (interpreter, argument) = (pageContent.interpreter, pageContent.argument.mkString(" "))
    pageContent.interpreter match {

      case Some("Comment") | Some("comment") => ""
      case Some("Text") | Some("text") | Some("txt") => "<pre>" + body.replaceAll("""&""", "&amp;").replaceAll("<", "&lt;") + "</pre>"
      case Some("Html") | Some("html") => body
      case Some("Markdown") | Some("markdown") => com.github.rjeschke.txtmark.Processor.process(body)

      case Some("Wiki") | Some("wiki") | None => new InterpreterWiki().render(body)

      case Some("WikiSyntaxPreview") => InterpreterWikiSyntaxPreview.render(pageContent)

      case Some("Paper") => InterpreterPaper.render(argument, body)
      case Some("Vim") | Some("vim") | Some("AhaTracVim") => InterpreterVim.render(pageContent)
      case Some("Table") | Some("table") | Some("AhaTracTable") => InterpreterTable.render(pageContent)
      case Some("Quote") | Some("quote") | Some("AhaTracQuote") => "<blockquote>" + new InterpreterWiki().render(body) + "</blockquote>"
      case Some("Math") => InterpreterMath.render(argument, body)
      case Some("Graph") => InterpreterGraph.render(pageContent)
      case _ =>
        Logger.error(s"$pageContent")
        "Error!" + s"$pageContent"
    }
  }
}
