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

  def doApply(argument: String, preprocessor:String => String)(implicit wikiContext: ContextWikiPage, connection: Connection): String = {
    implicit val provider: RequestWrapper = wikiContext.requestWrapper
    implicit val site: Site = wikiContext.site
    val pageLastRevision = models.tables.Page.selectLastRevision(argument)
    if (WikiPermission().isReadable(argument, pageLastRevision.map(s => PageContent(s.content)))) {
      pageLastRevision.map(w => Interpreters.toHtmlString(preprocessor(w.content))).getOrElse("Error: " + argument)
    } else {
      MacroError.toHtmlString(s"Permission Denied - [[$name($argument)]]")
    }
  }

  override def toSeqLink(argument: String)(implicit wikiContext: ContextWikiPage): Seq[CalculatedLink] =
    toCalculatedLinks(Seq(argument))
}
