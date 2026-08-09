package logics.wikis.macros

import models.ContextWikiPage
import models.tables.CalculatedLink

object MacroEmbed extends TraitMacro {
  override def isBlock: Boolean = true

  import scala.util.matching.Regex

  val regexYouTube: Regex = """(?:youtube(?:-nocookie)?\.com/(?:[^/\n\s]+/\S+/|(?:v|e(?:mbed)?)/|\S*?[?&]v=)|youtu\.be/)([a-zA-Z0-9_-]{11})""".r
  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    regexYouTube.findFirstMatchIn(argument).map(m => m.group(1)).map(views.html.Wiki.youtube(_).toString())
      .getOrElse(argumentError(argument))
  }

  override def toSeqLink(argument: String)(implicit wikiContext: ContextWikiPage): Seq[CalculatedLink] =
    toCalculatedLinks(Seq(argument))
}
