package logics.wikis.macros

import java.time.LocalDate

import com.aha00a.commons.utils.{DateTimeFormatterHolder, DateTimeUtil}
import logics.wikis.interpreters.InterpreterWiki
import logics.wikis.interpreters.InterpreterWiki.LinkMarkup
import models.WikiContext

import scala.util.Try

object MacroLinkDate extends TraitMacro {
  override def apply(argument: String)(implicit wikiContext: WikiContext): String = argument match {
    case "" | null => apply(wikiContext.name)
    case DateTimeUtil.regexYearDashMonth(y, m) => ""
    case DateTimeUtil.regexIsoLocalDate(y, m, d) =>
      val links = Seq(
        s"${l(s"$y-$m-$d")}",
        s"${l(s"$y-$m")}-${l(s"----${d}")}",
        s"${l(y)}-${l(s"--$m-$d")}",
        s"${l(y)}-${l(s"--$m")}-${l(s"----$d")}",
        ""
      ).mkString("<br/>")
      s"""
         |<div class="MacroLinkDate">
         |$links
         |</div>
         |""".stripMargin
    case _ => MacroError(s"Argument Error - [[$name($argument)]]")
  }

  @scala.annotation.tailrec
  override def extractLink(body: String)(implicit wikiContext: WikiContext): Seq[String] = body match {
    case "" | null => extractLink(wikiContext.name)
    case DateTimeUtil.regexIsoLocalDate(y, m, d) => Seq() // TODO
    case _ => Seq()
  }

  def l(s:String)(implicit wikiContext: WikiContext): String = LinkMarkup(s, s.replaceAllLiterally("--", "")).toHtmlString()

}
