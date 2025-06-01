package logics.wikis.interpreters

import logics.wikis.macros.MacroError
import models.{PageContent, ContextWikiPage}

object Interpreters extends TraitInterpreter {

  import models.tables.Link
  import models.tables.CalculatedSchemaOrg

  val map: Map[String, TraitInterpreter] = Seq(
    InterpreterWiki,
    InterpreterPaper,

    InterpreterWikiSyntaxPreview,

    InterpreterComment,
    InterpreterHtml,
    InterpreterText,
    InterpreterMarkdown,
    InterpreterQuote,
    InterpreterFold,

    InterpreterVim,
    InterpreterTable,
    InterpreterGraph,
    InterpreterMath,
    InterpreterMap,
    InterpreterMermaid,

    InterpreterSchema,
  ).map(m => m.name.toLowerCase -> m).toMap + ("AhaTracQuote".toLowerCase -> InterpreterQuote)

  def getInterpreter(pageContent: PageContent): Option[TraitInterpreter] = {
    map.get(pageContent.interpreter.map(_.toLowerCase).getOrElse("wiki"))
  }

  override def toHtmlString(content: String)(implicit wikiContext: ContextWikiPage): String = {
    val pageContent: PageContent = PageContent(content)
    getInterpreter(pageContent)
      .map(_.toHtmlString(content))
      .getOrElse(MacroError.toHtmlString(s"Interpreter not found.<br/><pre>[[[$content]]]</pre>"))
  }

  override def toSeqLink(content: String)(implicit wikiContext: ContextWikiPage): Seq[Link] = {
    val pageContent: PageContent = PageContent(content)
    getInterpreter(pageContent)
      .map(_.toSeqLink(content))
      .getOrElse(Seq())
  }

  override def toSeqSchemaOrg(content: String)(implicit wikiContext: ContextWikiPage): Seq[CalculatedSchemaOrg] = {
    val pageContent: PageContent = PageContent(content)
    getInterpreter(pageContent)
      .map(_.toSeqSchemaOrg(content))
      .getOrElse(Seq())
  }
}
