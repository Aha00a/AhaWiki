package logics.wikis.interpreters

import com.aha00a.commons.Implicits._
import logics.wikis.macros.MacroError
import models.{PageContent, ContextWikiPage}
import play.api.Logging

import scala.util.control.NonFatal

object Interpreters extends TraitInterpreter with Logging {

  import models.tables.CalculatedLink
  import models.tables.CalculatedSchemaOrg

  val map: Map[String, TraitInterpreter] = Seq(
    InterpreterWiki,
    InterpreterPaper,
    InterpreterSlide,

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
    InterpreterGantt,

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
        .filterNot(d => d == "var" || d.startsWith("var ") || d.startsWith("var\t"))
        .flatMap(_.split("""\s+"""))
        .find(_.nonEmpty)
    }
  }

  override def toHtmlString(content: String)(implicit wikiContext: ContextWikiPage): String = {
    val pageContent: PageContent = PageContent(content)
    getInterpreter(pageContent)
      .map(render(_, content))
      .getOrElse(MacroError.toHtmlString(s"Interpreter not found.<br/><pre>[[[$content]]]</pre>"))
  }

  /**
   * One block's failure stays in that block. Until 2026-09-26 an exception here went all the way
   * up and the whole page answered 500 -- an empty `#!Map`, a `#!Vim` block that could not run vi,
   * each took every other block on the page down with it. The owner decided a block that cannot
   * render shows an error box where it stands, like a block whose interpreter is not found, and
   * the page around it lives. The exception is logged in full; the box names it.
   */
  def render(interpreter: TraitInterpreter, content: String)(implicit wikiContext: ContextWikiPage): String =
    try interpreter.toHtmlString(content)
    catch {
      case NonFatal(e) =>
        logger.error(s"#!${interpreter.name} failed on page '${wikiContext.name}'", e)
        MacroError.toHtmlString(s"#!${interpreter.name} failed - ${e.toString.escapeHtml()}")
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
