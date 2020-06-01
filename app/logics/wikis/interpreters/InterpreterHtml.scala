package logics.wikis.interpreters

import models.{Link, PageContent, WikiContext}
import org.jsoup.Jsoup

object InterpreterHtml extends TraitInterpreter {
  override def toHtmlString(content: String)(implicit wikiContext: WikiContext): String = {
    val pageContent: PageContent = PageContent(content)
    pageContent.content
  }

  override def toSeqWord(content: String)(implicit wikiContext: WikiContext): Seq[String] = {
    Jsoup.parse(content).text().split("""\s+""")
  }

  override def toSeqLink(content: String)(implicit wikiContext: WikiContext): Seq[Link] = Seq()
}
