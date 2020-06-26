package logics.wikis.macros

import java.time.LocalDate

import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.{DateTimeUtil, RangeUtil}
import logics.wikis.interpreters.InterpreterWiki.LinkMarkup
import models.WikiContext

object MacroLinkDate extends TraitMacro {
  @scala.annotation.tailrec
  override def toHtmlString(argument: String)(implicit wikiContext: WikiContext): String = argument match {
    case "" | null => toHtmlString(wikiContext.name)
    case DateTimeUtil.regexYearDashMonth(y, m) => ""
    case DateTimeUtil.regexIsoLocalDate(y, m, d) =>
      val links = Seq(
        s"${l(s"$y-$m-$d")}",
        s"${l(s"$y-$m")}-${l(s"----$d")}",
        s"${l(y)}-${l(s"--$m-$d")}",
        s"${l(y)}-${l(s"--$m")}-${l(s"----$d")}",
        ""
      ).mkString("<br/>")
      val seqLinkAround: Seq[LinkMarkup] = getSeqLinkAround(argument)
      val linkAroundHtml = seqLinkAround.map(_.toHtmlString()).mkString("<br/>")
      s"""
         |<div class="MacroLinkDate around">
         |$linkAroundHtml
         |</div>
         |<div class="MacroLinkDate">
         |$links
         |</div>
         |""".stripMargin
    case _ => MacroError.toHtmlString(s"Argument Error - [[$name($argument)]]")
  }

  private def getSeqLinkAround(argument: String)(implicit wikiContext: WikiContext): Seq[LinkMarkup] = {
    val localDate: LocalDate = LocalDate.parse(argument)
    val seqLinkAround: Seq[LinkMarkup] = RangeUtil.around(0, 3).map(i => LinkMarkup(localDate.plusDays(i).toIsoLocalDateString))
    seqLinkAround
  }

  @scala.annotation.tailrec
  override def extractLink(body: String)(implicit wikiContext: WikiContext): Seq[String] = body match {
    case "" | null => extractLink(wikiContext.name)
    case DateTimeUtil.regexIsoLocalDate(y, m, d) =>
      DateTimeUtil.expand_ymd_to_ymd_ym_y_md_m_d(body)// ++ getSeqLinkAround(body).map(_.uri)
    case _ => Seq()
  }

  def l(s:String)(implicit wikiContext: WikiContext): String = LinkMarkup(s, s.replace("--", "")).toHtmlString()

}
