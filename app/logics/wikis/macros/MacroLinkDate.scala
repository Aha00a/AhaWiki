package logics.wikis.macros

import java.time.LocalDate

import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.{DateTimeUtil, RangeUtil}
import logics.wikis.interpreters.ahaMark.AhaMarkLink
import models.ContextWikiPage
import models.tables.CalculatedLink

object MacroLinkDate extends TraitMacro {
  override def isBlock: Boolean = true
  @scala.annotation.tailrec
  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = argument match {
    case "" | null => toHtmlString(wikiContext.name)
    case DateTimeUtil.regexYearDashMonth(_, _) => ""
    case DateTimeUtil.regexIsoLocalDate(y, m, d) =>
      val links = Seq(
        s"${l(s"$y-$m-$d")}",
        s"${l(s"$y-$m")}-${l(s"---$d")}",
        s"${l(y)}-${l(s"--$m-$d")}",
        s"${l(y)}-${l(s"--$m")}-${l(s"---$d")}",
        ""
      ).mkString("<br/>")
      val seqLinkAround: Seq[AhaMarkLink] = getSeqLinkAround(argument)
      val linkAroundHtml = seqLinkAround.map(_.toHtmlString()).mkString("<br/>")
      s"""
         |<div class="MacroLinkDate around">
         |$linkAroundHtml
         |</div>
         |<div class="MacroLinkDate">
         |$links
         |</div>
         |""".stripMargin
    case _ => argumentError(argument)
  }

  private def getSeqLinkAround(argument: String)(implicit wikiContext: ContextWikiPage): Seq[AhaMarkLink] = {
    val localDate: LocalDate = LocalDate.parse(argument)
    val seqLinkAround: Seq[AhaMarkLink] = RangeUtil.around(0, 3).map(i => AhaMarkLink(localDate.plusDays(i).toIsoLocalDateString, "", noFollow = true))
    seqLinkAround
  }

  override def toSeqLink(body: String)(implicit wikiContext: ContextWikiPage): Seq[CalculatedLink] =
    toCalculatedLinks(linkDestinations(body))

  @scala.annotation.tailrec
  // See the note in MacroCalendar: same name and shape, different expansion, deliberately apart.
  private def linkDestinations(body: String)(implicit wikiContext: ContextWikiPage): Seq[String] = body match {
    case "" | null => linkDestinations(wikiContext.name)
    case DateTimeUtil.regexYear(y) => (1 to 12).map(m => f"$y-${m.toString.padLeft(2, "0")}")
    case DateTimeUtil.regexYearDashMonth(_, _) => DateTimeUtil.expand_ym_to_ym_y(body)
    case DateTimeUtil.regexIsoLocalDate(_, _, _) => DateTimeUtil.expand_ymd_to_ymd_ym_y_md(body)
    case _ => Seq()
  }

  def l(s:String)(implicit wikiContext: ContextWikiPage): String = AhaMarkLink(s, s.replaceAll("^-+", ""), noFollow = true).toHtmlString()

}
