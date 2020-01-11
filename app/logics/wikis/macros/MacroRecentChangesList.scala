package logics.wikis.macros

import com.aha00a.supercsv.SupercsvUtil
import logics.wikis.PageLogic
import logics.wikis.interpreters.InterpreterWiki
import models.{PageWithoutContentWithSize, WikiContext}
import play.api.cache.CacheApi
import play.api.db.Database

import scala.util.matching.Regex

object MacroRecentChangesList extends TraitMacro {
  val regexDigits: Regex = """^(\d+)$""".r
  override def apply(argument:String)(implicit wikiContext: WikiContext): String = {
    implicit val cacheApi: CacheApi = wikiContext.cacheApi
    implicit val database: Database = wikiContext.database
    argument match {
      case "" | null => interpret(wikiContext.listPageByPermission.sortBy(_.dateTime).reverse)
      case regexDigits(i) => interpret(wikiContext.listPageByPermission.sortBy(_.dateTime).reverse.take(i.toInt))
      case _ => MacroError(s"Bad argument - [[$name($argument)]]")
    }
  }

  def interpret(list: List[PageWithoutContentWithSize])(implicit wikiContext: WikiContext): String = {
    def desc[T : Ordering]: Ordering[T] = implicitly[Ordering[T]].reverse
    val rows = list.sortBy(_.dateTime)(desc).map(t => Seq(
      s"""'''["${t.name}"]'''""",
      s"""["${t.name}?action=diff&after=${t.revision}" ${t.revision}]""",
      s"${t.toIsoLocalDateTimeString}",
      s"[${t.author}](${t.remoteAddress})",
      s"${t.comment}"
    ))

    InterpreterWiki(
      s"""[[[#!Table tsv 1 tablesorter
         |Name	Revision	at	by	comment
         |${SupercsvUtil.toTsvString(rows)}
         |]]]
         |""".stripMargin
    )
  }
}
