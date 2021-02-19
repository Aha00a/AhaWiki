package logics.wikis.macros

import models.ContextWikiPage

trait TraitMacro {
  val name: String = getClass.getSimpleName.replaceAll("^Macro", "").replaceAll("""\$$""", "")

  def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = argument

  // TODO fix to return Seq[Link]
  // TODO: rename to toSeqLink
  def extractLink(argument: String)(implicit wikiContext: ContextWikiPage): Seq[String] = Seq()
}
