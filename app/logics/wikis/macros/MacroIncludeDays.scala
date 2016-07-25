package logics.wikis.macros

import java.text.SimpleDateFormat
import java.time.format.TextStyle
import java.time.{LocalDateTime, YearMonth}
import java.util.Locale

import com.aha00a.commons.utils.LocalDateTimeUtil
import models.WikiContext

object MacroIncludeDays extends TraitMacro {
  val regex = """^(\d{4})-(\d{2})$""".r
  override def apply(argument: String)(implicit wikiContext: WikiContext): String = argument match {
    case "" | null => apply(wikiContext.name)
    case "-" => apply(wikiContext.name + ",-")
    case regex(y, m) => MacroDays.extractLinkExistsOnly(argument).reverse.map(pageName => MacroInclude.doApply(pageName, content => {
      val ldt: LocalDateTime = LocalDateTimeUtil.convert(new SimpleDateFormat("yyyy-MM-dd").parse(pageName))
      content
        .split("\n")
        .map(_.replaceAll("^(=+ )", "=$1"))
        .map(_.replaceAll("^== (.+)", s"== [$pageName] " + ldt.getDayOfWeek.getDisplayName(TextStyle.SHORT, Locale.KOREA)))
        .mkString("\n")
    })).mkString("\n")
    case _ => MacroError.apply(s"Argument Error - [[$name($argument)]]")
  }

  override def calcLength(body: String)(implicit wikiContext: WikiContext): Long = extractLink(body).mkString("\n").length

  override def extractLink(body: String)(implicit wikiContext: WikiContext): Seq[String] = body match {
    case "" | null => extractLink(wikiContext.name)
    case "-" => extractLink(wikiContext.name + ",-")
    case regex(y, m) => (1 to YearMonth.of(y.toInt, m.toInt).lengthOfMonth()).map(d => f"$y-${m.toInt}%02d-$d%02d")
    case _ => Seq()
  }
}
