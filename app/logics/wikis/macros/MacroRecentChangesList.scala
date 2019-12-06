package logics.wikis.macros

import com.aha00a.commons.implicits.Implicits._
import logics.Cache
import logics.wikis.interpreters.InterpreterWiki
import models.AhaWikiDatabase.PageNameRevisionTimeAuthorRemoteAddressSizeComment
import models.WikiContext

import scala.util.matching.Regex

object MacroRecentChangesList extends TraitMacro {
  val regexDigits: Regex = """^(\d+)$""".r
  override def apply(argument:String)(implicit wikiContext: WikiContext): String = argument match {
    case "" | null => interpret(Cache.PageList.get()(wikiContext.cacheApi, wikiContext.db).sortBy(_.time).reverse)
    case regexDigits(i) => interpret(Cache.PageList.get()(wikiContext.cacheApi, wikiContext.db).sortBy(_.time).reverse.take(i.toInt))
    case _ => MacroError.apply(s"Bad argument - [[$name($argument)]]")
  }

  def interpret(list: List[PageNameRevisionTimeAuthorRemoteAddressSizeComment])(implicit wikiContext: WikiContext): String = {
    new InterpreterWiki().apply(list.map(p => s" * ${p.localDateTime.toIsoLocalDateTimeString} - [${p.name}]").mkString("\n"))
  }
}
