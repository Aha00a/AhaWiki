package logics.wikis.macros

import com.aha00a.commons.utils.DateTimeUtil
import logics.wikis.interpreters.InterpreterWiki
import models.WikiContext

object MacroLinkDate extends TraitMacro {
  override def apply(argument: String)(implicit wikiContext: WikiContext): String = argument match {
    case "" | null => apply(wikiContext.name)
    case DateTimeUtil.regexIsoLocalDate(y, m, d) => argument // TODO
    case _ => MacroError(s"Argument Error - [[$name($argument)]]")
  }

  @scala.annotation.tailrec
  override def extractLink(body: String)(implicit wikiContext: WikiContext): Seq[String] = body match {
    case "" | null => extractLink(wikiContext.name)
    case DateTimeUtil.regexIsoLocalDate(y, m, d) => Seq() // TODO
    case _ => Seq()
  }
}
