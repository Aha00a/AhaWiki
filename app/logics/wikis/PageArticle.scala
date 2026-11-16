package logics.wikis

import logics.wikis.interpreters.InterpreterSchema
import logics.wikis.interpreters.InterpreterWiki
import models.ContextWikiPage
import models.PageContent
import play.api.libs.json.JsObject

/**
 * A page's `#!Schema Article` block and the Article view.scala.html writes for every page are the
 * same thing: the page itself, as an article. Until 2026-11-16 both were written, under different
 * `@id`s that did not point at each other, so a search engine read two articles on one page. Now
 * the block's properties are folded into the page's Article (theirs win), and the block writes no
 * JSON-LD of its own.
 *
 * Only the first such block is read. Two Article blocks on one page would be two articles, which a
 * page is not; the second is shown but not written.
 */
object PageArticle {
  /** Article or any of its subclasses (TechArticle, BlogPosting, ...). */
  def isArticle(schemaClass: String): Boolean = logics.CalculatedSchemaOrg.getClassHierarchy(schemaClass).contains("Article")

  def describesThePage(parseResult: InterpreterSchema.ParseResult): Boolean = parseResult.schemaClasses.exists(isArticle)

  /** What the page's Article block writes: `@type` and the properties, without page defaults. */
  def properties(content: String)(implicit wikiContext: ContextWikiPage): Option[JsObject] = {
    val pageContent = PageContent(content)
    if (pageContent.redirect.isDefined) {
      None
    } else {
      InterpreterWiki.blocks(pageContent)
        .map(PageContent(_))
        .filter(_.interpreter.exists(_.equalsIgnoreCase(InterpreterSchema.name)))
        .map(InterpreterSchema.parse)
        .find(describesThePage)
        .flatMap(InterpreterSchema.toJsonLdObject(_))
        .map(_ - "@context")
    }
  }
}
