package logics.wikis.macros

import logics.wikis.PageSection
import logics.wikis.WikiPermission
import logics.wikis.interpreters.Interpreters
import models.ContextWikiPage
import models.PageContent
import models.RequestWrapper
import models.tables.CalculatedLink
import models.tables.Site

import java.sql.Connection

object MacroInclude extends TraitMacro {
  override def isBlock: Boolean = true
  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    wikiContext.withConnection { implicit connection =>
      doApply(argument, s => s)
    }
  }

  /**
   * Renders another page here, under a context that knows it is being included.
   *
   * The included page renders under `wikiContext.push(argument)` rather than under the caller's
   * own context. Two things follow from that. A macro can tell where it is — `DayHeader` renders
   * a day page as a section when it is included and as a page when it is not — and a page that
   * includes an ancestor of itself is caught here rather than by the stack running out.
   *
   * The context is passed explicitly. Making it an `implicit val` puts two of them in scope,
   * which is ambiguous rather than nearest-wins.
   */
  def doApply(argument: String, preprocessor:String => String)(implicit wikiContext: ContextWikiPage, connection: Connection): String = {
    implicit val provider: RequestWrapper = wikiContext.requestWrapper
    implicit val site: Site = wikiContext.site
    val (pageName, heading) = split(argument)
    if (wikiContext.seqName.contains(pageName)) {
      MacroError.toHtmlString(s"Circular Include - ${macroCall(argument)}")
    } else {
      val pageLastRevision = models.tables.Page.selectLastRevision(pageName)
      if (WikiPermission().isReadable(pageName, pageLastRevision.map(s => PageContent(s.content)))) {
        val included: ContextWikiPage = wikiContext.push(pageName)
        pageLastRevision.map(w => heading match {
          case None => Interpreters.toHtmlString(preprocessor(w.content))(included)
          case Some(title) => PageSection.extract(w.content, title)(included)
            .map(section => Interpreters.toHtmlString(preprocessor(section))(included))
            .getOrElse(MacroError.toHtmlString(s"Section Not Found - ${macroCall(argument)}"))
        }).getOrElse("Error: " + argument)
      } else {
        MacroError.toHtmlString(s"Permission Denied - ${macroCall(argument)}")
      }
    }
  }

  /**
   * The page an argument names, and the heading in it if the argument asks for one: `Page#Heading`.
   *
   * A page whose whole name is the argument wins over a split. `C#` is a page; reading it as page
   * `C` with an empty heading would send the include to a page that does not exist. Only when
   * nothing has the whole name is the last `#` taken as the divider, so a heading that itself
   * contains `#` still has to be asked for with the page name that precedes the last one.
   */
  def split(argument: String)(implicit wikiContext: ContextWikiPage, connection: Connection): (String, Option[String]) = {
    implicit val site: Site = wikiContext.site
    val at = argument.lastIndexOf('#')
    if (at <= 0 || at == argument.length - 1 || models.tables.Page.selectLastRevision(argument).isDefined) {
      (argument, None)
    } else {
      (argument.substring(0, at), Some(argument.substring(at + 1)))
    }
  }

  override def toSeqLink(argument: String)(implicit wikiContext: ContextWikiPage): Seq[CalculatedLink] =
    toCalculatedLinks(Seq(wikiContext.withConnection { implicit connection => split(argument)._1 }))
}
