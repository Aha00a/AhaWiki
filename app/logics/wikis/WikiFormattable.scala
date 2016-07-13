package logics.wikis

import models.WikiContext

trait WikiFormattable {
  def apply(body:String)(implicit wikiContext: WikiContext): String = ""
  def calcLength(body:String)(implicit wikiContext: WikiContext):Long = 0
  def extractLink(body:String)(implicit wikiContext: WikiContext):Seq[String] = Seq()
}
