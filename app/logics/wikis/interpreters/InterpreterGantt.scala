package logics.wikis.interpreters

import models.{ContextWikiPage, PageContent}
import models.tables.CalculatedLink
import scala.util.Try

object InterpreterGantt extends TraitInterpreter {
  override def toHtmlString(content: String)(implicit wikiContext: ContextWikiPage): String = {
    val pageContent = PageContent(content)
    val id = com.aha00a.commons.utils.UuidUtil.newString
    val defaultEst = pageContent.variables
      .get("defaultEst")
      .map(_.trim)
      .flatMap(value => Try(value.toInt).toOption)
      .filter(_ > 0)
      .getOrElse(1)
    views.html.Wiki.InterpreterGantt(id, pageContent.content, defaultEst).toString()
  }

  override def toText(content: String)(implicit wikiContext: ContextWikiPage): String = {
    val pageContent = PageContent(content)
    pageContent.content
  }

  override def toSeqLink(content: String)(implicit wikiContext: ContextWikiPage): Seq[CalculatedLink] = Seq()
}
