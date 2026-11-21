package logics.wikis

import logics.wikis.interpreters.InterpreterSchema
import play.api.libs.json.JsObject
import play.api.libs.json.JsString
import play.api.libs.json.Json

/**
 * A page's `#!Schema Rating` block is the site's own rating of what the page is about, so it is
 * written as a Review of that thing rather than as a Rating on its own.
 *
 * Until 2026-11-22 the block wrote a Rating with the page's defaults -- `@id`, `url`, `name` --
 * which are the work's. A JSON-LD reader merges nodes that share an `@id`, so the page's work
 * became a work and a Rating at once, and nothing said the rating was of the work. It also had no
 * `bestRating`, which schema.org takes to be 5: every 9.0 on aha00a.com was nine out of five.
 *
 * The wiki text stays one line (`ratingValue 9.0`). Nesting the rating inside a Review block was
 * not done: it would make every rating heavier to write and every existing one wrong.
 */
object PageRating {
  /** What a rating is out of when the block does not say. Every rating written so far is out of 10. */
  val defaultBestRating: String = "10"

  /** Only Rating itself: AggregateRating and the other subclasses mean something else. */
  def isRating(parseResult: InterpreterSchema.ParseResult): Boolean = parseResult.schemaClasses == Seq("Rating")

  /**
   * The Review the block stands for. `itemReviewed` is the page URL, which is the `@id` every other
   * block on the page writes for its thing; `author` is the site, as it is for the page's Article.
   */
  def toReviewJsonLd(parseResult: InterpreterSchema.ParseResult, pageUrl: Option[String], host: Option[String]): Option[JsObject] =
    InterpreterSchema.toJsonLdObject(parseResult).map { rating =>
      val reviewRating = (rating - "@context") ++
        (if ((rating \ "bestRating").isDefined) Json.obj() else Json.obj("bestRating" -> defaultBestRating))
      Json.obj("@context" -> "https://schema.org", "@type" -> "Review") ++
        JsObject(Seq(
          pageUrl.map(url => "@id" -> JsString(s"$url#review")),
          pageUrl.map(url => "itemReviewed" -> Json.obj("@id" -> url)),
          host.map(h => "author" -> Json.obj("@id" -> JsonLdId.organization(h))),
        ).flatten) ++
        Json.obj("reviewRating" -> reviewRating)
    }
}
