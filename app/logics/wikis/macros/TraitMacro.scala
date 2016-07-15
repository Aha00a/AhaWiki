package logics.wikis.macros

import models.WikiContext

trait TraitMacro {
  val name = getClass.getSimpleName.replaceAll("^Macro", "").replaceAll("""\$$""", "")
  def apply(argument:String)(implicit wikiContext: WikiContext): String = argument
  def calcLength(argument:String)(implicit wikiContext: WikiContext):Long = argument.length
  def extractLink(argument:String)(implicit wikiContext: WikiContext):Seq[String] = Seq()
}