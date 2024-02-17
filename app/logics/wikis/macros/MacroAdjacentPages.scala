package logics.wikis.macros

import com.aha00a.commons.Implicits._
import logics.wikis.interpreters.InterpreterWiki
import models.ContextWikiPage

import java.sql.Connection

object MacroAdjacentPages extends TraitMacro {
  override def toHtmlString(argument:String)(implicit wikiContext: ContextWikiPage): String = {
    getMarkupRelatedPages(argument.getOrElse(wikiContext.nameTop))
  }

  def getMarkupRelatedPages(name: String)(implicit wikiContext: ContextWikiPage): String = {
    views.html.Wiki.adjacentPages(enableWikiLink = true).toString()
  }
}
