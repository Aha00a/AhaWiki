package logics.wikis.macros

import logics.Cache
import logics.wikis.{Interpreters, WikiPermission}
import models.Database.PageNameRevisionTimeAuthorRemoteAddressSizeComment
import models.{MockDb, PageContent, WikiContext}

object MacroIncludeStartsWith extends TraitMacro {
  override def apply(argument: String)(implicit wikiContext: WikiContext): String = argument match {
    case "" | null => apply(wikiContext.name)
    case _ =>
      val list: List[PageNameRevisionTimeAuthorRemoteAddressSizeComment] = Cache.PageList.get()
      list.filter(p => p.name != argument && p.name.startsWith(argument)).map(page => {
        val pageLastRevision = MockDb.selectPageLastRevision(page.name)
        if (WikiPermission.isReadable(pageLastRevision.map(s => new PageContent(s.content)))) {
          pageLastRevision.map(w => Interpreters.interpret(w.content.replaceFirst("""^= .+""", s"== [${w.name}]"))).getOrElse("Error: " + argument)
        } else {
          s"Failed to include $argument. Permission Denied" // TODO: add MacroError
        }
      }).mkString("\n")
  }

  override def extractLink(argument: String)(implicit wikiContext: WikiContext): Seq[String] = Seq(argument)
}
