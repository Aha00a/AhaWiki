package logics.wikis.macros

import models.WikiContext

trait TraitMacro {
  def apply(body:String)(implicit wikiContext: WikiContext): String = body
  def calcLength(body:String)(implicit wikiContext: WikiContext):Long = body.length
  def extractLink(body:String)(implicit wikiContext: WikiContext):Seq[String] = Seq()
}
