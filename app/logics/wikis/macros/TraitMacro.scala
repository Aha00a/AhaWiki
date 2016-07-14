package logics.wikis.macros

import models.Database.Link
import models.WikiContext

trait TraitMacro {
  def apply(argument:String)(implicit wikiContext: WikiContext): String = argument
  def calcLength(argument:String)(implicit wikiContext: WikiContext):Long = argument.length
  def extractLink(argument:String)(implicit wikiContext: WikiContext):Seq[Link] = Seq()
}
