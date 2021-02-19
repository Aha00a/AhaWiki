package logics.wikis.macros

import com.aha00a.supercsv.SupercsvUtil
import logics.wikis.PageLogic
import logics.wikis.interpreters.InterpreterWiki
import models.WikiContext
import play.api.cache.SyncCacheApi
import play.api.db.Database

import scala.util.matching.Regex

object MacroRecentChangesList extends TraitMacro {

  import models.tables.PageWithoutContentWithSize

  val regexDigits: Regex = """^(\d+)$""".r
  override def toHtmlString(argument:String)(implicit wikiContext: WikiContext): String = {
    implicit val database: Database = wikiContext.database
    def desc[T : Ordering]: Ordering[T] = implicitly[Ordering[T]].reverse
    argument match {
      case "" | null => toHtmlString(wikiContext.listPageByPermission.sortBy(_.dateTime)(desc))
      case regexDigits(i) => toHtmlString(wikiContext.listPageByPermission.sortBy(_.dateTime)(desc).take(i.toInt))
      case _ => MacroError.toHtmlString(s"Bad argument - [[$name($argument)]]")
    }
  }

  def toHtmlString(list: List[PageWithoutContentWithSize])(implicit wikiContext: WikiContext): String = {
    InterpreterWiki.toHtmlString(list.map(p => s""" * ${p.toIsoLocalDateTimeString} - ["${p.name}?action=diff&after=${p.revision}" r${p.revision}] - ["${p.name}"] - ${p.comment} by [${p.author}]""").mkString("\n"))
  }
}
