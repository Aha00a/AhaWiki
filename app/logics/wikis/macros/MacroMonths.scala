package logics.wikis.macros

import logics.wikis.interpreters.InterpreterWiki
import models.WikiContext

object MacroMonths extends TraitMacro {
  val regexIncr = """^(\d{4})$""".r
  val regexDecr = """^(\d{4}),-$""".r
  override def apply(argument: String)(implicit wikiContext: WikiContext): String = argument match {
    case "" | null => apply(wikiContext.name)
    case "-" => apply(wikiContext.name + ",-")
    case regexIncr(y) => new InterpreterWiki().apply((1 to 12).map(m => f" * [$y-$m%02d]").mkString("\n"))
    case regexDecr(y) => new InterpreterWiki().apply((1 to 12).reverse.map(m => f" * [$y-$m%02d]").mkString("\n"))
    case _ => s"Argument Error:($argument)"
  }

  override def calcLength(body: String)(implicit wikiContext: WikiContext): Long = extractLink(body).mkString("\n").length

  override def extractLink(body: String)(implicit wikiContext: WikiContext): Seq[String] = body match {
    case "" | null => extractLink(wikiContext.name)
    case "-" => extractLink(wikiContext.name + ",-")
    case regexIncr(y) => (1 to 12).map(m => f"$y-$m%02d")
    case regexDecr(y) => (1 to 12).reverse.map(m => f"$y-$m%02d")
    case _ => Seq()
  }
}
