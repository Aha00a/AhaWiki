package logics.wikis.macros

import com.aha00a.commons.implicits.Implicits._
import logics.AhaWikiCache
import logics.wikis.interpreters.InterpreterWiki
import models.AhaWikiDatabase.PageNameRevisionTimeAuthorRemoteAddressSizeComment
import models.WikiContext
import play.api.cache.CacheApi
import play.api.db.Database

import scala.util.matching.Regex

object MacroRecentChangesList extends TraitMacro {
  val regexDigits: Regex = """^(\d+)$""".r
  override def apply(argument:String)(implicit wikiContext: WikiContext): String = {
    implicit val cacheApi: CacheApi = wikiContext.cacheApi
    implicit val db: Database = wikiContext.database
    argument match {
      case "" | null => interpret(AhaWikiCache.PageList.get().sortBy(_.time).reverse)
      case regexDigits(i) => interpret(AhaWikiCache.PageList.get().sortBy(_.time).reverse.take(i.toInt))
      case _ => MacroError.apply(s"Bad argument - [[$name($argument)]]")
    }
  }

  def interpret(list: List[PageNameRevisionTimeAuthorRemoteAddressSizeComment])(implicit wikiContext: WikiContext): String = {
    new InterpreterWiki().apply(list.map(p => s" * ${p.localDateTime.toIsoLocalDateTimeString} - [${p.name}]").mkString("\n"))
  }
}
