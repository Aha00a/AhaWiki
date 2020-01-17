package logics.wikis.macros

import models.WikiContext

trait TraitMacro {
  val name: String = getClass.getSimpleName.replaceAll("^Macro", "").replaceAll("""\$$""", "")

  def apply(argument: String)(implicit wikiContext: WikiContext): String = argument

  def extractLink(argument: String)(implicit wikiContext: WikiContext): Seq[String] = Seq() // TODO fix to return Seq[Link]
}
