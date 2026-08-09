package logics.wikis.macros

import logics.wikis.interpreters.ahaMark.AhaMarkLink
import models.ContextWikiPage
import models.tables.CalculatedLink

trait TraitMacro {
  val name: String = getClass.getSimpleName.replaceAll("^Macro", "").replaceAll("""\$$""", "")
  def isBlock: Boolean = false

  def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = argument

  /** The macro call as the author wrote it, for error messages that quote it back. */
  protected def macroCall(argument: String): String = s"[[$name($argument)]]"

  /**
   * What a macro renders when it cannot make sense of its argument.
   *
   * Ten macros spelled this message out themselves. A wording change had to find all ten,
   * and a macro that phrased it differently looked like a different kind of failure to the
   * reader.
   */
  protected def argumentError(argument: String)(implicit wikiContext: ContextWikiPage): String =
    MacroError.toHtmlString(s"Argument Error - ${macroCall(argument)}")

  protected def toCalculatedLink(dst: String)(implicit wikiContext: ContextWikiPage): CalculatedLink =
    AhaMarkLink(dst).toLink(wikiContext.name)

  protected def toCalculatedLinks(dst: Seq[String])(implicit wikiContext: ContextWikiPage): Seq[CalculatedLink] =
    dst.map(toCalculatedLink)

  def toSeqLink(argument: String)(implicit wikiContext: ContextWikiPage): Seq[CalculatedLink] = Seq()
}
