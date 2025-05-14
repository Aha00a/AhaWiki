package logics.wikis.macros

import com.aha00a.commons.Implicits.RichPeriod
import com.aha00a.commons.Implicits.RichString
import com.aha00a.commons.utils.LocalDateUtil
import models.ContextWikiPage

import java.time.LocalDate
import java.time.Period

object MacroPeriod extends TraitMacro {
  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    argument
      .toOption
      .map(_.trim.split("""\s*,\s*""").toSeq.filter(_.isNotNullOrEmpty))
      .getOrElse(Seq[String]())
      .flatMap(v => LocalDateUtil.tryParse(v))
      .filter(_ != null)
    match {
      case Seq(localDate) => between(wikiContext.localDateNow, localDate)
      case Seq(localDate1, localDate2) => between(localDate1, localDate2)
      case _ => s"[[$name(${argument.orEmpty})]]"
    }
  }

  def between(localDate1: LocalDate, localDate2: LocalDate): String = {
    Period.between(localDate1, localDate2).toIso8601
  }
}
