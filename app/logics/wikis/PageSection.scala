package logics.wikis

import logics.wikis.interpreters.InterpreterWiki
import models.ContextWikiPage

/**
 * One section of a page's markup: a heading and everything under it, down to the next heading of
 * the same or a higher level.
 *
 * `[[Include(Page#Heading)]]` uses it. A section runs by level, not by the next heading of any
 * kind, so asking for `== Usage` brings its `=== Examples` along and stops at the next `== ` — the
 * same range the partial editor takes for the heading, and for the same reason: a section is what
 * the author would select with the mouse to move it.
 *
 * `[[[ ... ]]]` blocks are lifted out first. A line that begins `== ` inside a block is text the
 * author wrote — a transcript, a diff, a wiki-syntax example — not a heading, and without lifting it
 * would end a section early or be taken for the one that was asked for.
 */
object PageSection {
  /**
   * The section under the first heading whose title is `title`, heading line included, or None.
   *
   * The title is compared as written, trimmed. A heading that carries markup — `== [Dev]Deploying`
   * — is asked for with the markup, because that is what is on the page to copy; stripping it would
   * need a second opinion on what the renderer shows, and the two would drift.
   */
  def extract(markup: String, title: String)(implicit wikiContext: ContextWikiPage): Option[String] = {
    val blocks = ExtractConvertInject.markedBlocks(body => s"[[[$body]]]")
    val lines = blocks.extract(markup).split("""\r\n|\n""", -1)
    val headings: Seq[Option[(Int, String)]] = lines.toSeq.map {
      case InterpreterWiki.regexHeading(equals, _, text, _, _) => Some((equals.length, text.trim))
      case _ => None
    }

    val start = headings.indexWhere(_.exists(_._2 == title.trim))
    if (start < 0) {
      None
    } else {
      val level = headings(start).get._1
      val end = headings.indices
        .drop(start + 1)
        .find(i => headings(i).exists(_._1 <= level))
        .getOrElse(lines.length)
      Some(blocks.inject(lines.slice(start, end).mkString("\n")))
    }
  }
}
