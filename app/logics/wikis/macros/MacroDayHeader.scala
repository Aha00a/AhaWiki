package logics.wikis.macros

import com.aha00a.commons.utils.DateTimeUtil
import logics.wikis.PageNameUrl
import logics.wikis.interpreters.ahaMark.AhaMarkLink
import models.ContextWikiPage
import models.tables.CalculatedLink

/**
 * The heading of a day page, in either of the two places a day page is read.
 *
 * On its own it is the page, and gets the page's `h1` plus the date navigation. Included into
 * the month page that gathers it, it is one section among thirty, so it drops to `h2` and the
 * navigation goes away — the month page carries its own.
 *
 * Which of the two it is comes from the include stack, not from the argument. That is the whole
 * reason [[models.ContextWikiPage]] keeps one.
 */
object MacroDayHeader extends TraitMacro {
  override def isBlock: Boolean = true
  @scala.annotation.tailrec
  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    argument match {
      case "" | null => toHtmlString(wikiContext.name)
      case DateTimeUtil.regexIsoLocalDate(y, m, d) if !wikiContext.isIncluded =>
        val ymd = s"$y-$m-$d"
        val ymLink = AhaMarkLink(s"$y-$m", s"$y-$m", noFollow = true).toHtmlString()
        val weekday = MacroWeekdayName.toHtmlString(ymd)
        s"""
           |${MacroLinkDate.toHtmlString(ymd)}
           |<h1>${ymLink}-$d $weekday</h1>
           |""".stripMargin
      case DateTimeUtil.regexIsoLocalDate(y, m, d) =>
        // The heading has to carry its own anchor and link, because a macro's output is opaque
        // to the document that included it: the interpreter numbers and anchors the headings it
        // parses, and never sees this one. Hence the empty `headingNumber` — the section is
        // still addressable and still links to the day page, it just has no ordinal. The month
        // page's own heading renders the same way, so the shape is not new.
        val ymd = s"$y-$m-$d"
        val weekday = MacroWeekdayName.toHtmlString(ymd)
        val id = s"$ymd-$weekday"
        s"""
           |<h2 id="$id"><a href="#$id" class="headingNumber"></a> <a href="/w/${PageNameUrl.encode(ymd)}">$ymd</a> $weekday</h2>
           |""".stripMargin
      case _ => argumentError(argument)
    }
  }

  @scala.annotation.tailrec
  override def toSeqLink(body: String)(implicit wikiContext: ContextWikiPage): Seq[CalculatedLink] = body match {
    case "" | null => toSeqLink(wikiContext.name)
    case DateTimeUtil.regexIsoLocalDate(_, _, _) => MacroLinkDate.toSeqLink(body)
    case _ => Seq()
  }
}
