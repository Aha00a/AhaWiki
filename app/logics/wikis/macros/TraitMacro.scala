package logics.wikis.macros

import logics.wikis.interpreters.ahaMark.AhaMarkLink
import models.ContextWikiPage
import models.tables.CalculatedLink

trait TraitMacro {
  val name: String = getClass.getSimpleName.replaceAll("^Macro", "").replaceAll("""\$$""", "")
  def isBlock: Boolean = false

  def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = argument

  protected def toCalculatedLink(dst: String)(implicit wikiContext: ContextWikiPage): CalculatedLink =
    AhaMarkLink(dst).toLink(wikiContext.name)

  protected def toCalculatedLinks(dst: Seq[String])(implicit wikiContext: ContextWikiPage): Seq[CalculatedLink] =
    dst.map(toCalculatedLink)

  def toSeqLink(argument: String)(implicit wikiContext: ContextWikiPage): Seq[CalculatedLink] = Seq()
}
