package logics.wikis.macros

import java.time.format.TextStyle
import java.time.{DayOfWeek, YearMonth}
import java.util.Locale

import logics.wikis.interpreters.InterpreterTable
import models.{PageContent, WikiContext}

object MacroCalendar extends TraitMacro {
  val regex = """^(\d{4})-(\d{2})$""".r

  override def apply(argument: String)(implicit wikiContext: WikiContext): String = argument match {
    case "" | null => apply(wikiContext.name)
    case "-" => apply(wikiContext.name + ",-")
    case regex(y, m) =>
      val yearMonth = YearMonth.of(y.toInt, m.toInt)
      val header = DayOfWeek.values().map(_.getDisplayName(TextStyle.SHORT, Locale.getDefault))
      val firstPadding = Seq.fill(yearMonth.atDay(1).getDayOfWeek.getValue - 1)("")
      val lastPadding = Seq.fill(7 - yearMonth.atEndOfMonth().getDayOfWeek.getValue)("")
      val dates = (1 to yearMonth.lengthOfMonth()).map(d => s"[$argument-$d $d]")
      InterpreterTable.interpret(new PageContent("#!Table tsv 1\n"+ (header ++ firstPadding ++ dates ++ lastPadding).grouped(7).map(_.mkString("\t")).mkString("\n")))
    case _ => s"Argument Error:($argument)"
  }

  override def calcLength(body: String)(implicit wikiContext: WikiContext): Long = extractLink(body).mkString("\n").length

  override def extractLink(body: String)(implicit wikiContext: WikiContext): Seq[String] = body match {
    case "" | null => extractLink(wikiContext.name)
    case "-" => extractLink(wikiContext.name + ",-")
    case regex(y, m) => (1 to YearMonth.of(y.toInt, m.toInt).lengthOfMonth()).map(d => f"$y-${m.toInt}%02d-$d%02d")
    case _ => Seq()
  }

}
