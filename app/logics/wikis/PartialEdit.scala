package logics.wikis

import models.ContextWikiPage
import models.tables.Page
import models.tables.Site

/**
 * Editing part of a page rather than the whole of it.
 *
 * A rendered page carries an edit link per section, and the link has to name the revision it
 * was rendered from along with the line range to open. Both halves of that — which revision,
 * and what the link looks like — were written out twice, once in
 * [[ExtractConvertInjectInterpreter]] for interpreter blocks and once in
 * [[logics.wikis.interpreters.InterpreterWiki]] for headings. The two renderers chunk a page
 * differently, which is why they stay apart; what they cannot disagree about is the link.
 */
object PartialEdit {

  /**
   * The revision the reader is looking at.
   *
   * The query string wins, because a reader viewing an old revision should get edit links
   * into that revision rather than into the current one. With no revision in the URL it is
   * the page's latest.
   *
   * Carrying the revision in the link is what lets the save detect that the page moved under
   * the editor while a section was open.
   */
  def revision(implicit wikiContext: ContextWikiPage): Long = {
    wikiContext.requestWrapper
      .getQueryString("revision")
      .flatMap(v => scala.util.Try(v.toLong).toOption)
      .filter(_ > 0)
      .getOrElse {
        val (database, site) = wikiContext.tupleDatabaseSite
        database.withConnection { implicit connection =>
          implicit val implicitSite: Site = site
          Page.selectLastRevision(wikiContext.name).map(_.revision).getOrElse(0L)
        }
      }
  }

  /** The link a section's edit control points at. */
  def editUrl(revision: Long, lineStart: Int, lineEnd: Int)(implicit wikiContext: ContextWikiPage): String =
    editUrl(wikiContext.name, revision, lineStart, lineEnd)

  /**
   * The same link, for a section that came from somewhere else.
   *
   * A document assembled out of other pages — see [[InlinedSource]] — has sections that belong
   * to those pages rather than to the one being read, and an edit control naming the page being
   * read would open the wrong thing, or nothing.
   */
  def editUrl(page: String, revision: Long, lineStart: Int, lineEnd: Int): String =
    s"/w/${PageNameUrl.encode(page)}?action=edit&revision=$revision&lineStart=$lineStart&lineEnd=$lineEnd"
}
