package logics.wikis.macros

import logics.Cache
import models.WikiContext

trait TraitMacro {
  val name: String = getClass.getSimpleName.replaceAll("^Macro", "").replaceAll("""\$$""", "")
  def apply(argument:String)(implicit wikiContext: WikiContext): String = argument
  def calcLength(argument:String)(implicit wikiContext: WikiContext):Long = argument.length
  def extractLink(argument:String)(implicit wikiContext: WikiContext):Seq[String] = Seq()
  def extractLinkExistsOnly(argument:String)(implicit wikiContext: WikiContext):Seq[String] = extractLink(argument).filter(existsInPageName)
  def existsInPageName(implicit wikiContext: WikiContext): String => Boolean = Cache.PageNameSet.get()(wikiContext.cacheApi).contains
}
