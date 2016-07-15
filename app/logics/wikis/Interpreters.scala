package logics.wikis

import logics.wikis.interpreters._
import models.Database.Link
import models.{PageContent, WikiContext}
import play.api.Logger

object Interpreters {
  def interpret(s: String)(implicit wikiContext: WikiContext): String = {
    val pageContent: PageContent = new PageContent(s)
    val body = pageContent.content
    val (interpreter, argument) = (pageContent.interpreter, pageContent.argument.mkString(" "))
    pageContent.interpreter match {

      case Some("Comment") | Some("comment") => ""
      case Some("Text") | Some("text") | Some("txt") => "<pre>" + body.replaceAll( """&""", "&amp;").replaceAll("<", "&lt;") + "</pre>"
      case Some("Html") | Some("html") => body
      case Some("Markdown") | Some("markdown") => InterpreterMarkdown(body)

      case Some("Wiki") | Some("wiki") | None => (new InterpreterWiki())(body)

      case Some("WikiSyntaxPreview") => InterpreterWikiSyntaxPreview.interpret(pageContent)

      case Some("Paper") => InterpreterPaper.interpret(argument, body)
      case Some("Vim") | Some("vim") | Some("AhaTracVim") => InterpreterVim.interpret(pageContent)
      case Some("Table") | Some("table") | Some("AhaTracTable") => InterpreterTable.interpret(pageContent)
      case Some("Quote") | Some("quote") | Some("AhaTracQuote") => "<blockquote>" + new InterpreterWiki().apply(body) + "</blockquote>"
      case Some("Math") => InterpreterMath(argument, body)
      case Some("Graph") => InterpreterGraph(pageContent)
      case _ =>
        Logger.error(s"$pageContent")
        "Error!" + s"$pageContent"
    }
  }


  def extractLink(name:String, content: String)(implicit wikiContext: WikiContext): Seq[Link] = {
    val pageContent: PageContent = new PageContent(content)
    val body = pageContent.content
    val (interpreter, argument) = (pageContent.interpreter, pageContent.argument.mkString(" "))
    pageContent.interpreter match {

//      case Some("Comment") | Some("comment") => ""
//      case Some("Text") | Some("text") | Some("txt") => "<pre>" + body.replaceAll( """&""", "&amp;").replaceAll("<", "&lt;") + "</pre>"
//      case Some("Html") | Some("html") => body
//      case Some("Markdown") | Some("markdown") => com.github.rjeschke.txtmark.Processor.process(body)
//
      case Some("Wiki") | Some("wiki") | None => new InterpreterWiki().extractLink(name, body).filterNot(_.or(_.startsWith("#"))).toSeq
//
//      case Some("WikiSyntaxPreview") => InterpreterWikiSyntaxPreview.interpret(pageContent)
//
//      case Some("Paper") => InterpreterPaper.interpret(argument, body)
//      case Some("Vim") | Some("vim") | Some("AhaTracVim") => InterpreterVim.interpret(pageContent)
//      case Some("Table") | Some("table") | Some("AhaTracTable") => InterpreterTable.interpret(pageContent)
//      case Some("Quote") | Some("quote") | Some("AhaTracQuote") => "<blockquote>" + new InterpreterWiki().interpret(body) + "</blockquote>"
//      case Some("Math") => InterpreterMath.interpret(argument, body)
//      case Some("Graph") => InterpreterGraph.interpret(pageContent)
      case _ => Seq()
    }
  }
}
