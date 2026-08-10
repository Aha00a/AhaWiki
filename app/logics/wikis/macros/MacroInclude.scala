package logics.wikis.macros

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
    wikiContext.database.withConnection { implicit connection =>
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
    if (wikiContext.seqName.contains(argument)) {
      MacroError.toHtmlString(s"Circular Include - ${macroCall(argument)}")
    } else {
      val pageLastRevision = models.tables.Page.selectLastRevision(argument)
      if (WikiPermission().isReadable(argument, pageLastRevision.map(s => PageContent(s.content)))) {
        val included: ContextWikiPage = wikiContext.push(argument)
        pageLastRevision.map(w => Interpreters.toHtmlString(preprocessor(w.content))(included)).getOrElse("Error: " + argument)
      } else {
        MacroError.toHtmlString(s"Permission Denied - ${macroCall(argument)}")
      }
    }
  }

  override def toSeqLink(argument: String)(implicit wikiContext: ContextWikiPage): Seq[CalculatedLink] =
    toCalculatedLinks(Seq(argument))
}
