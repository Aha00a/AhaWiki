package logics.wikis.macros

import logics.AhaWikiCache
import logics.wikis.WikiPermission
import logics.wikis.interpreters.Interpreters
import models.PageWithoutContentWithSize
import models.{AhaWikiQuery, PageContent, WikiContext}

object MacroIncludeStartsWith extends TraitMacro {                 // TODO: design & implement
  override def apply(argument: String)(implicit wikiContext: WikiContext): String = argument match {
    case "" | null => apply(wikiContext.name)
    case _ => wikiContext.database.withConnection { implicit connection =>
      val list: List[PageWithoutContentWithSize] = AhaWikiCache.PageList.get()(wikiContext.cacheApi, wikiContext.database)
      list.filter(p => p.name != argument && p.name.startsWith(argument)).map(page => {
        val pageLastRevision = AhaWikiQuery().Page.selectLastRevision(page.name)
        if (WikiPermission.isReadable(pageLastRevision.map(s => PageContent(s.content)))(wikiContext.request, wikiContext.cacheApi, wikiContext.database)) {
          pageLastRevision.map(w => Interpreters.interpret(w.content.replaceFirst("""^= .+""", s"== [${w.name}]"))).getOrElse("Error: " + argument)
        } else {
          MacroError(s"Permission Denied - [[$name($argument)]]")
        }
      }).mkString("\n")
    }
  }

  override def extractLink(argument: String)(implicit wikiContext: WikiContext): Seq[String] = Seq(argument)
}
