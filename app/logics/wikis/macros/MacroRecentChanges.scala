package logics.wikis.macros

import models.ContextWikiPage
import views.html.macros

object MacroRecentChanges extends TraitMacro {
  override def isBlock: Boolean = true

  private def parseLimit(argument: String): Option[Int] =
    Option(argument)
      .map(_.trim)
      .filter(_.nonEmpty)
      .flatMap(raw => scala.util.Try(raw.toInt).toOption)
      .map(_.max(1).min(1000))

  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    macros.RecentChanges(parseLimit(argument))(wikiContext).toString
  }
}
