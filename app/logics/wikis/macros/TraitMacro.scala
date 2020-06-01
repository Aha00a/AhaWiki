package logics.wikis.macros

import models.WikiContext

trait TraitMacro {
  val name: String = getClass.getSimpleName.replaceAll("^Macro", "").replaceAll("""\$$""", "")

  // TODO: rename to toHtmlString
  def apply(argument: String)(implicit wikiContext: WikiContext): String = argument

  // TODO fix to return Seq[Link]
  // TODO: rename to toSeqLink
  def extractLink(argument: String)(implicit wikiContext: WikiContext): Seq[String] = Seq()
}
