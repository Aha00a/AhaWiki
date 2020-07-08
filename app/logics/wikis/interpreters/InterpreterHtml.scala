package logics.wikis.interpreters

import models.{PageContent, WikiContext}
import org.jsoup.Jsoup

object InterpreterHtml extends TraitInterpreter {

  import models.tables.Link

  override def toHtmlString(content: String)(implicit wikiContext: WikiContext): String = {
    val pageContent: PageContent = PageContent(content)
    pageContent.content
  }

  override def toSeqWord(content: String)(implicit wikiContext: WikiContext): Seq[String] = {
    Jsoup.parse(content).text().split("""\s+""")
  }

  override def toSeqLink(content: String)(implicit wikiContext: WikiContext): Seq[Link] = Seq()
}
