package logics

import com.aha00a.commons.utils.DateTimeUtil
import models.tables.SearchResult

/**
 * Pages that say this page's name in their text without linking to it — what a link would be
 * worth adding to. Obsidian calls them unlinked mentions.
 *
 * It is a filter over the search the wiki already has. `Page.pageSearch` finds every page whose
 * name or text contains a string; what is left to decide is which of those are mentions *of this
 * page* and not yet links *to* it. That rule is pure, so it lives here where it can be tested, and
 * the database is asked nothing new.
 */
object UnlinkedMentions {
  /** How many rows of search to read before filtering. Pages that already link are dropped afterwards. */
  val ScanLimit = 500
  val ShowLimit = 50

  /**
   * Whether a name is worth looking for. A name of one or two characters is a substring of half the
   * wiki, and a date page's name appears in the text of every page that mentions a date — both
   * would answer with noise and cost a scan of every page for it.
   */
  def searchable(name: String): Boolean = {
    val n = name.trim
    n.length >= 3 &&
      DateTimeUtil.regexIsoLocalDate.findFirstIn(n).isEmpty &&
      DateTimeUtil.regexYearDashMonth.findFirstIn(n).isEmpty &&
      DateTimeUtil.regexYear.findFirstIn(n).isEmpty &&
      DateTimeUtil.regexDashDashMonthDashDay.findFirstIn(n).isEmpty
  }

  case class Mention(name: String, snippet: String)

  /**
   * The mentions among `found`, newest first as the search gave them.
   *
   * A page is dropped if it is the page itself, if it already links here (`linkSources`), or if
   * the name is only in *its* name and not its text — the search matches either, and a page called
   * "Foo Bar" is not a mention of "Foo". `canRead` is the reader's permission, asked per page.
   */
  def of(name: String, found: Seq[SearchResult], linkSources: Set[String], canRead: SearchResult => Boolean): Seq[Mention] = {
    val needle = name.trim.toLowerCase
    found
      .filter(r => r.name != name)
      .filterNot(r => linkSources.contains(r.name))
      .filter(r => r.content.toLowerCase.contains(needle))
      .filter(canRead)
      .map(r => Mention(r.name, SearchSnippet.of(r.content, name)))
  }
}
