package logics.wikis.interpreters

import models.ContextWikiPage

trait TraitInterpreter {

  import models.tables.CalculatedLink
  import models.tables.CalculatedSchemaOrg
  import org.jsoup.Jsoup

  val name: String = getClass.getSimpleName.replaceAll("^Interpreter", "").replaceAll("""\$$""", "")

  def toHtmlString(content: String)(implicit wikiContext: ContextWikiPage): String

  def toSeqLink(content: String)(implicit wikiContext: ContextWikiPage): Seq[CalculatedLink]

  def toSeqSchemaOrg(content: String)(implicit wikiContext: ContextWikiPage): Seq[CalculatedSchemaOrg] = Seq()

  def toText(content: String)(implicit wikiContext: ContextWikiPage): String = {
    val document = Jsoup.parse(toHtmlString(content))
    document.select(".headingNumber").remove()
    // document.select(".schema dt").remove()
    document.text()
  }
}

