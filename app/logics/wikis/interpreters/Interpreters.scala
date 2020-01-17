package logics.wikis.interpreters

import logics.wikis.macros.MacroError
import models.{Link, PageContent, WikiContext}
import play.api.Logger

object Interpreters {
  val map: Map[String, TraitInterpreter] = Seq(
    InterpreterWiki,
    InterpreterSchema,
    InterpreterComment,
    InterpreterHtml
  ).map(m => m.name.toLowerCase -> m).toMap

  def interpret(content: String)(implicit wikiContext: WikiContext): String = {
    val pageContent: PageContent = PageContent(content)
    val body = pageContent.content
    val argument = pageContent.argument.mkString(" ")

    getInterpreter(pageContent) match {
      case Some(interpreter) => interpreter.interpret(content)
      case None =>
        pageContent.interpreter match {

          case Some("Graph") => InterpreterGraph(pageContent)
          case Some("Map") => InterpreterMap(pageContent)
          case Some("Markdown") | Some("markdown") => InterpreterMarkdown(body)
          case Some("Math") => InterpreterMath(argument, body)
          case Some("Paper") => InterpreterPaper.interpret(argument, body)
          case Some("Quote") | Some("quote") | Some("AhaTracQuote") => "<blockquote>" + InterpreterWiki.interpret(body) + "</blockquote>"
          case Some("Table") | Some("table") => InterpreterTable.interpret(pageContent)
          case Some("Text") | Some("text") | Some("txt") => "<pre class=\"text\">" + body.replaceAll( """&""", "&amp;").replaceAll("<", "&lt;") + "</pre>"
          case Some("Vim") | Some("vim") => InterpreterVim.interpret(pageContent)
          case Some("WikiSyntaxPreview") => InterpreterWikiSyntaxPreview.interpret(pageContent)
          case _ =>
            Logger.error(s"$pageContent")
            MacroError(s"Interpreter not found.<br/><pre>[[[$content]]]</pre>")
        }
    }
  }


  def extractLink(content: String)(implicit wikiContext: WikiContext): Seq[Link] = {
    val pageContent: PageContent = PageContent(content)
    getInterpreter(pageContent).map(_.extractLink(content)).getOrElse(Seq())
  }

  def getInterpreter(pageContent: PageContent): Option[TraitInterpreter] = {
    map.get(pageContent.interpreter.map(_.toLowerCase).getOrElse("wiki"))
  }
}
