package logics.wikis.macros

import com.aha00a.commons.Implicits._
import logics.wikis.interpreters.Interpreters
import models.ContextWikiPage
import models.tables.Site
import play.api.db.Database

import java.text.SimpleDateFormat
import java.time.format.TextStyle
import java.time.LocalDateTime
import java.time.YearMonth
import scala.util.matching.Regex

object MacroIncludeDays extends TraitMacro {
  val regex: Regex = """^(\d{4})-(\d{2})$""".r

  //noinspection ScalaUnusedSymbol
  @scala.annotation.tailrec
  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = argument match {
    case "" | null => toHtmlString(wikiContext.name)
    case "-" => toHtmlString(wikiContext.name + ",-")
    case regex(y, m) =>
      implicit val database: Database = wikiContext.database
      implicit val site: Site = wikiContext.site

      val set = wikiContext.setPageNameByPermission
      val seq = getSeqDays_yyyy_dash_MM_dash_dd(y.toInt, m.toInt).filter(set.contains)
      if (seq.isEmpty) {
        ""
      } else {
        wikiContext.database.withConnection { implicit connection =>
          val content = models.tables.Page.selectLastRevision(seq).map { p =>
            val ldt: LocalDateTime = new SimpleDateFormat("yyyy-MM-dd").parse(p.name).toLocalDateTime
            val weekday = ldt.getDayOfWeek.getDisplayName(TextStyle.SHORT, wikiContext.requestWrapper.locale)
            s"== [${p.name}] $weekday\n" + p.content
              .split("\n")
              .tail
              .map(_.replaceAll("^(=+ )", "=$1"))
              .mkString("\n")
          }.mkString("\n")
          Interpreters.toHtmlString(content)
        }
      }
    case _ => MacroError.toHtmlString(s"Argument Error - [[$name($argument)]]")
  }

  @scala.annotation.tailrec
  override def extractLink(body: String)(implicit wikiContext: ContextWikiPage): Seq[String] = body match {
    case "" | null => extractLink(wikiContext.name)
    case "-" => extractLink(wikiContext.name + ",-")
    case regex(y, m) => (1 to YearMonth.of(y.toInt, m.toInt).lengthOfMonth()).map(d => f"$y-${m.toInt}%02d-$d%02d")
    case _ => Seq()
  }

  private def getSeqDays(y: Int, m: Int): Seq[Int] = 1 to YearMonth.of(y, m).lengthOfMonth()
  private def getSeqDays_yyyy_dash_MM_dash_dd(y: Int, m: Int): Seq[String] = getSeqDays(y, m).map(d => f"$y%04d-$m%02d-$d%02d")
}
