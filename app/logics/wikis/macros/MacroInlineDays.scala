package logics.wikis.macros

import com.aha00a.commons.Implicits._
import logics.wikis.ExtractConvertInject
import logics.wikis.interpreters.Interpreters
import models.ContextWikiPage
import models.PageContent
import models.tables.CalculatedLink
import models.tables.Site
import play.api.db.Database

import java.text.SimpleDateFormat
import java.time.format.TextStyle
import java.time.LocalDateTime
import java.time.YearMonth
import scala.util.matching.Regex

/**
 * The day pages of a month, spliced into this page and rendered with it as one document.
 *
 * Inline, not include, and the distinction is load-bearing. [[MacroInclude]] renders another
 * page as that page: it pushes onto the include stack, so the included content's edit links,
 * attachment keys and structured data name the page it came from. This does the opposite — it
 * takes the day pages' markup and makes it part of the month page, which is what puts the day
 * headings in the month page's own heading tree, and is where its numbering, its contents list
 * and its collapsible sections come from. Rendering each day separately would produce a page
 * that reads the same and has none of those.
 *
 * The cost of being an inline is that macros in the spliced markup see the month page rather
 * than the day they were written on. That follows from the choice rather than being a defect of
 * it: after splicing there is one document, and it is the month page. Anything that has to
 * belong to the day page — an attachment, a backlink — belongs in a page read on its own.
 */
object MacroInlineDays extends TraitMacro {
  override def isBlock: Boolean = true
  val regex: Regex = """^(\d{4})-(\d{2})$""".r

  //noinspection ScalaUnusedSymbol
  @scala.annotation.tailrec
  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = argument match {
    case "" | null => toHtmlString(wikiContext.name)
    case "-" => toHtmlString(wikiContext.name + ",-")
    case regex(y, m) =>
      implicit val database: Database = wikiContext.database
      implicit val site: Site = wikiContext.site

      val set = wikiContext.setPageNameByPermission
      val seq = getSeqDays_yyyy_dash_MM_dash_dd(y.toInt, m.toInt).filter(set.contains)
      if (seq.isEmpty) {
        ""
      } else {
        wikiContext.database.withConnection { implicit connection =>
          val content = models.tables.Page.selectLastRevision(seq).map { p =>
            val ldt: LocalDateTime = new SimpleDateFormat("yyyy-MM-dd").parse(p.name).toLocalDateTime
            val weekday = ldt.getDayOfWeek.getDisplayName(TextStyle.SHORT, wikiContext.requestWrapper.locale)
            s"== [${p.name}] $weekday\n" + oneLevelDeeper(bodyWithoutOwnHeading(p.content))
          }.mkString("\n")
          Interpreters.toHtmlString(content)
        }
      }
    case _ => argumentError(argument)
  }

  /**
   * A day page's body, without the heading it carries for when it is read on its own.
   *
   * Every day page opens with its own title — `[[DayHeader]]`, or the same thing written by
   * hand as `= [2020-01]-04 Sat` — and the month page supplies that heading itself, so keeping
   * the page's own would print it twice.
   *
   * Only a heading is dropped. This used to take the first line whatever it happened to be,
   * which was safe only because no day page has ever opened with prose, and would have eaten
   * the first sentence of the one that did. Directives go through `PageContent`, which knows a
   * page may carry more than one of them.
   */
  private def bodyWithoutOwnHeading(raw: String): String = {
    val lines = PageContent(raw).content.split("\n").toSeq
    val ownHeading = lines.headOption.exists(l => l.startsWith("=") || l.contains(s"[[${MacroDayHeader.name}"))
    (if (ownHeading) lines.tail else lines).mkString("\n")
  }

  /**
   * The page's own headings, pushed one level down so they sit under the day's heading.
   *
   * The shift runs with interpreter blocks lifted out, because `== ` at the start of a line
   * inside a `[[[ ... ]]]` block is text the author wrote, not a heading. Shifting it corrupts
   * the block: a shell transcript or a diff gains an `=` it never had. No day page has such a
   * line today — this is a trap rather than a bug — but the raw version was one line of regex
   * away from silently editing quoted text, and the lifting already exists.
   */
  def oneLevelDeeper(markup: String)(implicit wikiContext: ContextWikiPage): String = {
    // extractByMarkers keeps only what is between the delimiters, so putting them back is the
    // restoring half of the round trip rather than an addition.
    val blocks = ExtractConvertInject.markedBlocks(body => s"[[[$body]]]")
    blocks.inject(blocks.extract(markup).replaceAll("(?m)^(=+ )", "=$1"))
  }

  override def toSeqLink(body: String)(implicit wikiContext: ContextWikiPage): Seq[CalculatedLink] =
    toCalculatedLinks(linkDestinations(body))

  @scala.annotation.tailrec
  // See the note in MacroCalendar: same name and shape, different expansion, deliberately apart.
  private def linkDestinations(body: String)(implicit wikiContext: ContextWikiPage): Seq[String] = body match {
    case "" | null => linkDestinations(wikiContext.name)
    case "-" => linkDestinations(wikiContext.name + ",-")
    case regex(y, m) => (1 to YearMonth.of(y.toInt, m.toInt).lengthOfMonth()).map(d => f"$y-${m.toInt}%02d-$d%02d")
    case _ => Seq()
  }

  private def getSeqDays(y: Int, m: Int): Seq[Int] = 1 to YearMonth.of(y, m).lengthOfMonth()
  private def getSeqDays_yyyy_dash_MM_dash_dd(y: Int, m: Int): Seq[String] = getSeqDays(y, m).map(d => f"$y%04d-$m%02d-$d%02d")
}
