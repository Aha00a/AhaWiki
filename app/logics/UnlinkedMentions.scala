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
    val word = wordMatch(name)
    found
      .filter(r => r.name != name)
      .filterNot(r => linkSources.contains(r.name))
      .filter(canRead)
      .flatMap { r =>
        val flat = SearchSnippet.flatten(r.content)
        word.findFirstMatchIn(flat).map(m => Mention(r.name, SearchSnippet.window(flat, m.start, m.end - m.start)))
      }
  }

  /**
   * The name where it stands on its own, not inside a longer name.
   *
   * `Macro` is a substring of `MacroInclude`, `MacroPageMap` and every other page in the family, and a
   * list of those is not a list of mentions. So a match with a Latin letter, a digit or an underscore
   * touching it on either side does not count. Hangul is left out of that rule on purpose: Korean
   * glues a particle to the noun (`성운을`, `성운이`), and refusing those would miss most mentions
   * written in it. The cost is the opposite miss — a Korean name inside a longer Korean word is found.
   */
  private def wordMatch(name: String): scala.util.matching.Regex =
    ("(?i)(?<![A-Za-z0-9_])" + java.util.regex.Pattern.quote(name.trim) + "(?![A-Za-z0-9_])").r
}
