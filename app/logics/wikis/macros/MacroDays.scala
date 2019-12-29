package logics.wikis.macros

import java.time.YearMonth

import logics.wikis.interpreters.InterpreterWiki
import models.WikiContext

import scala.util.matching.Regex

object MacroDays extends TraitMacro {
  val regexIncr: Regex = """^(\d{4})-(\d{2})$""".r
  val regexDecr: Regex = """^(\d{4})-(\d{2}),-$""".r
  override def apply(argument: String)(implicit wikiContext: WikiContext): String = argument match {
    case "" | null => apply(wikiContext.name)
    case "-" => apply(wikiContext.name + ",-")
    case regexIncr(y, m) => new InterpreterWiki()((1 to YearMonth.of(y.toInt, m.toInt).lengthOfMonth()).map(d => f" * [$y-${m.toInt}%02d-$d%02d]").mkString("\n"))
    case regexDecr(y, m) => new InterpreterWiki()((1 to YearMonth.of(y.toInt, m.toInt).lengthOfMonth()).reverse.map(d => f" * [$y-${m.toInt}%02d-$d%02d]").mkString("\n"))
    case _ => MacroError(s"Argument Error - [[$name($argument)]]")
  }

  override def calcLength(body: String)(implicit wikiContext: WikiContext): Long = extractLink(body).mkString("\n").length

  @scala.annotation.tailrec
  override def extractLink(body: String)(implicit wikiContext: WikiContext): Seq[String] = body match {
    case "" | null => extractLink(wikiContext.name)
    case "-" => extractLink(wikiContext.name + ",-")
    case regexIncr(y, m) => (1 to YearMonth.of(y.toInt, m.toInt).lengthOfMonth()).map(d => f"$y-${m.toInt}%02d-$d%02d")
    case regexDecr(y, m) => (1 to YearMonth.of(y.toInt, m.toInt).lengthOfMonth()).reverse.map(d => f"$y-${m.toInt}%02d-$d%02d")
    case _ => Seq()
  }
}
