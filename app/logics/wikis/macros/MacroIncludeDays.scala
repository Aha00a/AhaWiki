package logics.wikis.macros

import java.text.SimpleDateFormat
import java.time.format.TextStyle
import java.time.{LocalDateTime, YearMonth}

import com.aha00a.commons.Implicits._
import com.aha00a.play.Implicits._
import logics.AhaWikiCache
import models.WikiContext
import play.api.cache.CacheApi
import play.api.db.Database

import scala.util.matching.Regex

object MacroIncludeDays extends TraitMacro {
  val regex: Regex = """^(\d{4})-(\d{2})$""".r

  //noinspection ScalaUnusedSymbol
  override def apply(argument: String)(implicit wikiContext: WikiContext): String = argument match {
    case "" | null => apply(wikiContext.name)
    case "-" => apply(wikiContext.name + ",-")
    case regex(y, m) =>
      implicit val cacheApi: CacheApi = wikiContext.cacheApi
      implicit val database: Database = wikiContext.database
      val set = AhaWikiCache.PageNameSet.get()

      getSeqDays_yyyy_dash_MM_dash_dd(y.toInt, m.toInt).filter(set.contains).reverse.map(pageName => MacroInclude.doApply(pageName, content => {
        val ldt: LocalDateTime = new SimpleDateFormat("yyyy-MM-dd").parse(pageName).toLocalDateTime
        content
          .split("\n")
          .map(_.replaceAll("^(=+ )", "=$1"))
          .map(_.replaceAll("^== (.+)", s"== [$pageName] " + ldt.getDayOfWeek.getDisplayName(TextStyle.SHORT, wikiContext.request.locale)))
          .mkString("\n")
      })).mkString("\n")
    case _ => MacroError(s"Argument Error - [[$name($argument)]]")
  }

  @scala.annotation.tailrec
  override def extractLink(body: String)(implicit wikiContext: WikiContext): Seq[String] = body match {
    case "" | null => extractLink(wikiContext.name)
    case "-" => extractLink(wikiContext.name + ",-")
    case regex(y, m) => (1 to YearMonth.of(y.toInt, m.toInt).lengthOfMonth()).map(d => f"$y-${m.toInt}%02d-$d%02d")
    case _ => Seq()
  }

  private def getSeqDays(y: Int, m: Int): Seq[Int] = 1 to YearMonth.of(y, m).lengthOfMonth()
  private def getSeqDays_yyyy_dash_MM_dash_dd(y: Int, m: Int): Seq[String] = getSeqDays(y, m).map(d => f"$y%04d-$m%02d-$d%02d")
}
