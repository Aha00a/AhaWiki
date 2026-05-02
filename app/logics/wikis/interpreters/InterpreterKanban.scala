package logics.wikis.interpreters

import com.aha00a.commons.Implicits._
import models.ContextWikiPage
import models.tables.CalculatedLink

object InterpreterKanban extends TraitInterpreter {
  override def toHtmlString(content: String)(implicit wikiContext: ContextWikiPage): String = {
    s"""<pre class="Interpreter InterpreterKanban" data-kanban-test="TODO">${content.escapeHtml()}</pre>"""
  }

  override def toSeqLink(content: String)(implicit wikiContext: ContextWikiPage): Seq[CalculatedLink] = Seq() // TODO
}
