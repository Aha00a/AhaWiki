package logics.wikis.macros

import com.aha00a.commons.Implicits.RichString
import models.ContextWikiPage

object MacroKbd extends TraitMacro {
  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    doToHtmlString(parseKeys(argument))
  }

  def doToHtmlString(keys: Seq[String]): String = {
    val renderedKeys = keys.map(key => s"<kbd>${key.escapeHtml()}</kbd>").mkString
    s"""<kbd class="MacroKbd">$renderedKeys</kbd>"""
  }

  private[macros] def parseKeys(argument: String): Seq[String] = {
    argument.trim.split("""\s+""").toSeq
  }
}
