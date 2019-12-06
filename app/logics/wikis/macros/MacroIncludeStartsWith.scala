package logics.wikis.macros

import logics.Cache
import logics.wikis.{Interpreters, WikiPermission}
import models.AhaWikiDatabase.PageNameRevisionTimeAuthorRemoteAddressSizeComment
import models.{AhaWikiDatabase, PageContent, WikiContext}

object MacroIncludeStartsWith extends TraitMacro {                 // TODO: design & implement
  override def apply(argument: String)(implicit wikiContext: WikiContext): String = argument match {
    case "" | null => apply(wikiContext.name)
    case _ =>
      val list: List[PageNameRevisionTimeAuthorRemoteAddressSizeComment] = Cache.PageList.get()(wikiContext.cacheApi, wikiContext.db)
      list.filter(p => p.name != argument && p.name.startsWith(argument)).map(page => {
        val pageLastRevision = AhaWikiDatabase()(wikiContext.db).pageSelectLastRevision(page.name)
        if (WikiPermission.isReadable(pageLastRevision.map(s => PageContent(s.content)))(wikiContext.request, wikiContext.cacheApi, wikiContext.db)) {
          pageLastRevision.map(w => Interpreters.interpret(w.content.replaceFirst("""^= .+""", s"== [${w.name}]"))).getOrElse("Error: " + argument)
        } else {
          MacroError.apply(s"Permission Denied - [[$name($argument)]]")
        }
      }).mkString("\n")
  }

  override def extractLink(argument: String)(implicit wikiContext: WikiContext): Seq[String] = Seq(argument)
}
