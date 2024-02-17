package logics.wikis.macros

import com.aha00a.commons.Implicits._
import logics.wikis.interpreters.InterpreterWiki
import models.ContextWikiPage

import java.sql.Connection

object MacroAdjacentPages extends TraitMacro {
  import scala.util.matching.Regex

  val year: Regex = """\d{4}""".r
  val date: Regex = """\d{4}-\d{2}-\d{2}""".r

  override def toHtmlString(argument:String)(implicit wikiContext: ContextWikiPage): String = {
    getMarkupRelatedPages(argument.getOrElse(wikiContext.nameTop))
  }

  def getMarkupRelatedPages(name: String)(implicit wikiContext: ContextWikiPage): String = {
    views.html.Wiki.adjacentPages(enableWikiLink = true).toString()
  }
}
