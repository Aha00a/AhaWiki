package logics.wikis.macros

import logics.wikis.interpreters.InterpreterWiki
import models.WikiContext

object MacroMonths {
  val regexIncr = """^(\d{4})$""".r
  val regexDecr = """^(\d{4}),-$""".r
  def apply(argument: String)(implicit wikiContext: WikiContext): String = argument match {
    case "" | null => apply(wikiContext.name)
    case "-" => apply(wikiContext.name + ",-")
    case regexIncr(y) => new InterpreterWiki().interpret((1 to 12).map(m => f" * [$y-$m%02d]").mkString("\n"))
    case regexDecr(y) => new InterpreterWiki().interpret((1 to 12).reverse.map(m => f" * [$y-$m%02d]").mkString("\n"))
    case _ => s"Argument Error:($argument)"
  }
}
