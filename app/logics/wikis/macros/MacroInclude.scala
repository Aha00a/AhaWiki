package logics.wikis.macros

import logics.wikis.WikiPermission
import logics.wikis.interpreters.Interpreters
import models.{AhaWikiQuery, PageContent, WikiContext}

object MacroInclude extends TraitMacro {
  override def apply(argument: String)(implicit wikiContext: WikiContext): String = doApply(argument, s => s)
  def doApply(argument: String, preprocessor:String => String)(implicit wikiContext: WikiContext): String = { wikiContext.database.withConnection { implicit connection =>
    val pageLastRevision = AhaWikiQuery().Page.selectLastRevision(argument)
    if (WikiPermission()(wikiContext.request, wikiContext.cacheApi, wikiContext.database).isReadable(pageLastRevision.map(s => PageContent(s.content)))) {
      pageLastRevision.map(w => Interpreters.toHtmlString(preprocessor(w.content))).getOrElse("Error: " + argument)
    } else {
      MacroError(s"Permission Denied - [[$name($argument)]]")
    }
  }}

  override def extractLink(argument: String)(implicit wikiContext: WikiContext): Seq[String] = Seq(argument)
}
