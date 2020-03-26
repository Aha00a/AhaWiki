package logics.wikis.interpreters

import logics.wikis.macros.MacroError
import models.{Link, PageContent, SchemaOrg, WikiContext}

object Interpreters extends TraitInterpreter {
  val map: Map[String, TraitInterpreter] = Seq(
    InterpreterWiki,
    InterpreterPaper,

    InterpreterWikiSyntaxPreview,

    InterpreterComment,
    InterpreterHtml,
    InterpreterText,
    InterpreterMarkdown,
    InterpreterQuote,

    InterpreterVim,
    InterpreterTable,
    InterpreterGraph,
    InterpreterMath,
    InterpreterMap,
    
    InterpreterSchema,
    null
  ).filter(_ != null).map(m => m.name.toLowerCase -> m).toMap + ("AhaTracQuote".toLowerCase -> InterpreterQuote)

  def getInterpreter(pageContent: PageContent): Option[TraitInterpreter] = {
    map.get(pageContent.interpreter.map(_.toLowerCase).getOrElse("wiki"))
  }

  override def interpret(content: String)(implicit wikiContext: WikiContext): String = {
    val pageContent: PageContent = PageContent(content)
    getInterpreter(pageContent)
      .map(_.interpret(content))
      .getOrElse(MacroError(s"Interpreter not found.<br/><pre>[[[$content]]]</pre>"))
  }

  override def extractLink(content: String)(implicit wikiContext: WikiContext): Seq[Link] = {
    val pageContent: PageContent = PageContent(content)
    getInterpreter(pageContent)
      .map(_.extractLink(content))
      .getOrElse(Seq())
  }

  override def extractSchema(content: String)(implicit wikiContext: WikiContext): Seq[SchemaOrg] = {
    val pageContent: PageContent = PageContent(content)
    getInterpreter(pageContent)
      .map(_.extractSchema(content))
      .getOrElse(Seq())
  }
}
