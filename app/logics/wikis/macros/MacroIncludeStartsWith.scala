package logics.wikis.macros

import logics.wikis.interpreters.Interpreters
import logics.wikis.{PageLogic, WikiPermission}
import models.{AhaWikiQuery, PageContent, PageWithoutContentWithSize, WikiContext}

object MacroIncludeStartsWith extends TraitMacro {                 // TODO: design & implement
  override def toHtmlString(argument: String)(implicit wikiContext: WikiContext): String = argument match {
    case "" | null => toHtmlString(wikiContext.name)
    case _ => wikiContext.database.withConnection { implicit connection =>
      val list: List[PageWithoutContentWithSize] = wikiContext.listPageByPermission
      list.filter(p => p.name != argument && p.name.startsWith(argument)).map(page => {
        val pageLastRevision = AhaWikiQuery().Page.selectLastRevision(page.name)
        if (WikiPermission()(wikiContext.request, wikiContext.cacheApi, wikiContext.database).isReadable(pageLastRevision.map(s => PageContent(s.content)))) {
          pageLastRevision.map(w => Interpreters.toHtmlString(w.content.replaceFirst("""^= .+""", s"== [${w.name}]"))).getOrElse("Error: " + argument)
        } else {
          MacroError.toHtmlString(s"Permission Denied - [[$name($argument)]]")
        }
      }).mkString("\n")
    }
  }

  override def extractLink(argument: String)(implicit wikiContext: WikiContext): Seq[String] = Seq(argument)
}
