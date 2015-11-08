package logics.wikis.macros

import logics.wikis.{Interpreters, WikiPermission}
import models.{MockDb, PageContent, WikiContext}

object MacroInclude {
  def apply(argument: String)(implicit wikiContext: WikiContext): String = {
    val pageLastRevision = MockDb.selectPageLastRevision(argument)
    if (WikiPermission.isReadable(pageLastRevision.map(s => new PageContent(s.content)))) {
      pageLastRevision.map(w => Interpreters.interpret(w.content)).getOrElse("Error: " + argument)
    } else {
      s"Failed to include $argument. Permission Denied"
    }
  }
}
