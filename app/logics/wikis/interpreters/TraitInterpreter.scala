package logics.wikis.interpreters

import models.ContextWikiPage
import models.PageContent

trait TraitInterpreter {

  import models.tables.CalculatedLink
  import models.tables.CalculatedSchemaOrg
  import org.jsoup.Jsoup

  val name: String = getClass.getSimpleName.replaceAll("^Interpreter", "").replaceAll("""\$$""", "")

  def toHtmlString(content: String)(implicit wikiContext: ContextWikiPage): String

  /**
   * The same, for a caller that has already split the text into directives and body.
   * `Interpreters` has, to find the interpreter; building a `PageContent` splits the whole text
   * into lines, and until 2026-11-01 the interpreter it chose split it again. An interpreter
   * that keeps the given one saves that; the rest take the text as before.
   */
  def toHtmlString(pageContent: PageContent)(implicit wikiContext: ContextWikiPage): String = toHtmlString(pageContent.raw)

  def toSeqLink(content: String)(implicit wikiContext: ContextWikiPage): Seq[CalculatedLink]

  def toSeqSchemaOrg(content: String)(implicit wikiContext: ContextWikiPage): Seq[CalculatedSchemaOrg] = Seq()

  def toText(content: String)(implicit wikiContext: ContextWikiPage): String = {
    val document = Jsoup.parse(toHtmlString(content))
    document.select(".headingNumber").remove()
    // document.select(".schema dt").remove()
    document.text()
  }
}

