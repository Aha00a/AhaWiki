package logics.wikis.interpreters

import models.ContextWikiPage
import models.PageContent

trait TraitInterpreter {

  import models.tables.CalculatedLink
  import models.tables.CalculatedSchemaOrg
  import org.jsoup.Jsoup

  val name: String = getClass.getSimpleName.replaceAll("^Interpreter", "").replaceAll("""\$$""", "")

  def toHtmlString(content: String)(implicit wikiContext: ContextWikiPage): String

  def toSeqLink(content: String)(implicit wikiContext: ContextWikiPage): Seq[CalculatedLink]

  def toSeqSchemaOrg(content: String)(implicit wikiContext: ContextWikiPage): Seq[CalculatedSchemaOrg] = Seq()

  // The same three, for a caller that has already split the text into directives and body.
  // `Interpreters` has, to find the interpreter; building a `PageContent` splits the whole text
  // into lines, and until 2026-11-01 the interpreter it chose split it again. An interpreter
  // that works on the `PageContent` ([[TraitInterpreterOfPageContent]]) keeps the one it is
  // given; the rest take the text as before.
  def toHtmlString(pageContent: PageContent)(implicit wikiContext: ContextWikiPage): String = toHtmlString(pageContent.raw)

  def toSeqLink(pageContent: PageContent)(implicit wikiContext: ContextWikiPage): Seq[CalculatedLink] = toSeqLink(pageContent.raw)

  def toSeqSchemaOrg(pageContent: PageContent)(implicit wikiContext: ContextWikiPage): Seq[CalculatedSchemaOrg] = toSeqSchemaOrg(pageContent.raw)

  def toText(content: String)(implicit wikiContext: ContextWikiPage): String = toText(PageContent(content))

  def toText(pageContent: PageContent)(implicit wikiContext: ContextWikiPage): String = {
    val document = Jsoup.parse(toHtmlString(pageContent))
    document.select(".headingNumber").remove()
    // document.select(".schema dt").remove()
    document.text()
  }
}

/**
 * An interpreter whose work starts from the split text. It implements the `PageContent` forms;
 * the `String` forms split and delegate, here, once, rather than as a copy in each such
 * interpreter -- there were two the day this was written.
 */
trait TraitInterpreterOfPageContent extends TraitInterpreter {

  import models.tables.CalculatedLink
  import models.tables.CalculatedSchemaOrg

  override def toHtmlString(pageContent: PageContent)(implicit wikiContext: ContextWikiPage): String

  override def toSeqLink(pageContent: PageContent)(implicit wikiContext: ContextWikiPage): Seq[CalculatedLink]

  override def toSeqSchemaOrg(pageContent: PageContent)(implicit wikiContext: ContextWikiPage): Seq[CalculatedSchemaOrg]

  final override def toHtmlString(content: String)(implicit wikiContext: ContextWikiPage): String = toHtmlString(PageContent(content))

  final override def toSeqLink(content: String)(implicit wikiContext: ContextWikiPage): Seq[CalculatedLink] = toSeqLink(PageContent(content))

  final override def toSeqSchemaOrg(content: String)(implicit wikiContext: ContextWikiPage): Seq[CalculatedSchemaOrg] = toSeqSchemaOrg(PageContent(content))
}
