package logics.wikis.interpreters

import logics.wikis.macros.MacroError
import models.{PageContent, ContextWikiPage}

object Interpreters extends TraitInterpreter {

  import models.tables.CalculatedLink
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
    InterpreterKanban,

    InterpreterSchema,
  ).map(m => m.name.toLowerCase -> m).toMap + ("AhaTracQuote".toLowerCase -> InterpreterQuote)

  def getInterpreter(pageContent: PageContent): Option[TraitInterpreter] = {
    map.get(pageContent.interpreter.map(_.toLowerCase).getOrElse("wiki"))
  }

  def getInterpreter(content: String): Option[TraitInterpreter] = {
    map.get(interpreterName(content).getOrElse("wiki").toLowerCase)
  }

  private def interpreterName(content: String): Option[String] = {
    if (content == null) {
      None
    } else {
      val normalized = if (content.startsWith("\n#!")) content.substring(1) else content
      normalized
        .linesIterator
        .takeWhile(_.startsWith("#!"))
        .map(_.substring(2))
        .filterNot(_.startsWith("read"))
        .filterNot(_.startsWith("write"))
        .filterNot(_.startsWith("redirect"))
        .flatMap(_.split("""\s+"""))
        .find(_.nonEmpty)
    }
  }

  override def toHtmlString(content: String)(implicit wikiContext: ContextWikiPage): String = {
    val pageContent: PageContent = PageContent(content)
    getInterpreter(pageContent)
      .map(_.toHtmlString(content))
      .getOrElse(MacroError.toHtmlString(s"Interpreter not found.<br/><pre>[[[$content]]]</pre>"))
  }

  override def toSeqLink(content: String)(implicit wikiContext: ContextWikiPage): Seq[CalculatedLink] = {
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
