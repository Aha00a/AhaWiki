package logics.wikis.interpreters

import models.ContextWikiPage

object InterpreterComment extends TraitInterpreter {

  import models.tables.Link

  override def toHtmlString(content: String)(implicit wikiContext: ContextWikiPage): String = ""

  override def toSeqLink(content: String)(implicit wikiContext: ContextWikiPage): Seq[Link] = Seq()
}
