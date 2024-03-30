package logics.wikis.interpreters

import com.aha00a.commons.Implicits.RichString
import models.ContextWikiPage
import models.PageContent

object InterpreterFold extends TraitInterpreter {

  import models.tables.Link

  override def toHtmlString(content: String)(implicit wikiContext: ContextWikiPage): String = {
    val pageContent: PageContent = PageContent(content)
    val buttonText = pageContent.argument.mkString(" ").toOption.getOrElse("Fold / Unfold")

    views.html.Wiki.fold(buttonText, InterpreterWiki.toHtmlString(pageContent.content)).toString()
  }

  override def toSeqLink(content: String)(implicit wikiContext: ContextWikiPage): Seq[Link] = {
    val pageContent: PageContent = PageContent(content)
    InterpreterWiki.toSeqLink(pageContent.content)
  }
}
