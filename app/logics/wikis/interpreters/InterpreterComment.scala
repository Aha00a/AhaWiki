package logics.wikis.interpreters

import models.WikiContext

object InterpreterComment extends TraitInterpreter {

  import models.tables.Link

  override def toHtmlString(content: String)(implicit wikiContext: WikiContext): String = ""

  override def toSeqWord(content: String)(implicit wikiContext: WikiContext): Seq[String] = Seq()

  override def toSeqLink(content: String)(implicit wikiContext: WikiContext): Seq[Link] = Seq()
}
