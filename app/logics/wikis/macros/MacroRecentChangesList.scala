package logics.wikis.macros

import com.aha00a.commons.utils.IpAddressUtil
import logics.wikis.interpreters.InterpreterWiki
import models.ContextWikiPage
import play.api.db.Database

import scala.util.matching.Regex

object MacroRecentChangesList extends TraitMacro {

  import models.tables.PageWithoutContentWithSize

  val regexDigits: Regex = """^(\d+)$""".r
  override def toHtmlString(argument:String)(implicit wikiContext: ContextWikiPage): String = {
    def desc[T : Ordering]: Ordering[T] = implicitly[Ordering[T]].reverse
    argument match {
      case "" | null => toHtmlString(wikiContext.seqPageByPermission.sortBy(_.dateTime)(desc))
      case regexDigits(i) => toHtmlString(wikiContext.seqPageByPermission.sortBy(_.dateTime)(desc).take(i.toInt))
      case _ => MacroError.toHtmlString(s"Bad argument - [[$name($argument)]]")
    }
  }

  def toHtmlString(list: Seq[PageWithoutContentWithSize])(implicit wikiContext: ContextWikiPage): String = {
    InterpreterWiki.toHtmlString(list.map(p => s""" * ${p.toIsoLocalDateTimeString} - [[Html(<a rel="nofollow" href="/w/${p.name}?action=diff&after=${p.revision}">r${p.revision}</a>)]] - ["${p.name}"] - ${p.comment} by [${p.author.getOrElse(IpAddressUtil.mask(p.remoteAddress))}]""").mkString("\n"))
  }
}
