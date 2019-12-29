package logics.wikis.macros

import logics.wikis.interpreters.InterpreterWiki
import models.WikiContext

object MacroMonths extends TraitMacro {
  val regexIncr = """^(\d{4})$""".r
  val regexDecr = """^(\d{4}),-$""".r
  override def apply(argument: String)(implicit wikiContext: WikiContext): String = argument match {
    case "" | null => apply(wikiContext.name)
    case "-" => apply(wikiContext.name + ",-")
    case regexIncr(y) => InterpreterWiki((1 to 12).map(m => f" * [$y-$m%02d]").mkString("\n"))
    case regexDecr(y) => InterpreterWiki((1 to 12).reverse.map(m => f" * [$y-$m%02d]").mkString("\n"))
    case _ => MacroError(s"Argument Error - [[$name($argument)]]")
  }

  override def extractLink(body: String)(implicit wikiContext: WikiContext): Seq[String] = body match {
    case "" | null => extractLink(wikiContext.name)
    case "-" => extractLink(wikiContext.name + ",-")
    case regexIncr(y) => (1 to 12).map(m => f"$y-$m%02d")
    case regexDecr(y) => (1 to 12).reverse.map(m => f"$y-$m%02d")
    case _ => Seq()
  }
}
