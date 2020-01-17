package logics.wikis.interpreters

import logics.wikis.macros.MacroError
import models.{Link, PageContent, WikiContext}
import play.api.Logger

object Interpreters {
  val mapMacros: Map[String, TraitInterpreter] = Seq(
    InterpreterWiki,
    InterpreterSchema
  ).map(m => m.name.toLowerCase -> m).toMap

  def interpret(content: String)(implicit wikiContext: WikiContext): String = {
    val pageContent: PageContent = PageContent(content)
    val body = pageContent.content
    val argument = pageContent.argument.mkString(" ")
    pageContent.interpreter match {
      case Some("Wiki") | Some("wiki") | None => InterpreterWiki(body)
      case Some("Schema") | Some("schema") => InterpreterSchema(pageContent)

      case Some("Comment") | Some("comment") => InterpreterComment(pageContent)
      case Some("Graph") => InterpreterGraph(pageContent)
      case Some("Html") | Some("html") => body
      case Some("Map") => InterpreterMap(pageContent)
      case Some("Markdown") | Some("markdown") => InterpreterMarkdown(body)
      case Some("Math") => InterpreterMath(argument, body)
      case Some("Paper") => InterpreterPaper.interpret(argument, body)
      case Some("Quote") | Some("quote") | Some("AhaTracQuote") => "<blockquote>" + InterpreterWiki(body) + "</blockquote>"
      case Some("Table") | Some("table") => InterpreterTable.interpret(pageContent)
      case Some("Text") | Some("text") | Some("txt") => "<pre class=\"text\">" + body.replaceAll( """&""", "&amp;").replaceAll("<", "&lt;") + "</pre>"
      case Some("Vim") | Some("vim") => InterpreterVim.interpret(pageContent)
      case Some("WikiSyntaxPreview") => InterpreterWikiSyntaxPreview.interpret(pageContent)
      case _ =>
        Logger.error(s"$pageContent")
        MacroError(s"Interpreter not found.<br/><pre>[[[$content]]]</pre>")
    }
  }


  def extractLink(content: String)(implicit wikiContext: WikiContext): Seq[Link] = {
    val pageContent: PageContent = PageContent(content)
    mapMacros.get(pageContent.interpreter.map(_.toLowerCase).getOrElse("wiki")).map(_.extractLink(content)).getOrElse(Seq())
  }
}
