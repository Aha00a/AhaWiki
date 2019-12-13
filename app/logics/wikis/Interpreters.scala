package logics.wikis

import logics.wikis.interpreters._
import logics.wikis.macros.MacroError
import models.Link
import models.{PageContent, WikiContext}
import play.api.Logger

object Interpreters {
  def interpret(s: String)(implicit wikiContext: WikiContext): String = {
    val pageContent: PageContent = PageContent(s)
    val body = pageContent.content
    val argument = pageContent.argument.mkString(" ")
    pageContent.interpreter match {

      case Some("Comment") | Some("comment") => ""
      case Some("Text") | Some("text") | Some("txt") => "<pre class=\"text\">" + body.replaceAll( """&""", "&amp;").replaceAll("<", "&lt;") + "</pre>"
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
      case Some("Map") => InterpreterMap(pageContent)
      case _ =>
        Logger.error(s"$pageContent")
        MacroError.apply(s"Interpreter not found. - [[[$s]]]")
    }
  }


  def extractLink(name:String, content: String)(implicit wikiContext: WikiContext): Seq[Link] = {
    val pageContent: PageContent = PageContent(content)
    val body = pageContent.content
    pageContent.interpreter match {

//      case Some("Comment") | Some("comment") => ""
//      case Some("Text") | Some("text") | Some("txt") => "<pre>" + body.replaceAll( """&""", "&amp;").replaceAll("<", "&lt;") + "</pre>"
//      case Some("Html") | Some("html") => body
//      case Some("Markdown") | Some("markdown") => com.github.rjeschke.txtmark.Processor.process(body)
//
      case Some("Wiki") | Some("wiki") | None => new InterpreterWiki().extractLink(name, body).filterNot(_.or(_.startsWith("#")))
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
