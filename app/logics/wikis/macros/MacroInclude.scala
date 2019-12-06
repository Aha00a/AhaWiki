package logics.wikis.macros

import logics.wikis.{Interpreters, WikiPermission}
import models.{AhaWikiDatabase, MockDb, PageContent, WikiContext}

object MacroInclude extends TraitMacro {
  override def apply(argument: String)(implicit wikiContext: WikiContext): String = doApply(argument, s => s)
  def doApply(argument: String, preprocessor:String => String)(implicit wikiContext: WikiContext): String = {
    val pageLastRevision = AhaWikiDatabase()(wikiContext.db).pageSelectLastRevision(argument)
    if (WikiPermission.isReadable(pageLastRevision.map(s => PageContent(s.content)))) {
      pageLastRevision.map(w => Interpreters.interpret(preprocessor(w.content))).getOrElse("Error: " + argument)
    } else {
      MacroError.apply(s"Permission Denied - [[$name($argument)]]")
    }
  }

  override def extractLink(argument: String)(implicit wikiContext: WikiContext): Seq[String] = Seq(argument)
}
