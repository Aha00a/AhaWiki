package logics.wikis.macros

import com.aha00a.commons.utils.IpAddressUtil
import logics.wikis.UserPageLogic
import logics.wikis.interpreters.InterpreterWiki
import models.ContextWikiPage

object MacroRecentChangesList extends TraitMacro {

  import models.tables.PageWithoutContentWithSize

  private val includeMinorEditKeywords = Set("minor", "include-minor", "include_minor", "includeminor")

  private def parseArgument(argument: String): Either[String, (Option[Int], Boolean)] = {
    val tokens = Option(argument).map(_.trim).filter(_.nonEmpty).toSeq
      .flatMap(_.split(",").toSeq.map(_.trim).filter(_.nonEmpty))
      .map(_.toLowerCase)
    if (tokens.isEmpty) {
      Right((None, false))
    } else {
      val (sizeTokens, optionTokens) = tokens.partition(_.matches("""\d+"""))
      if (sizeTokens.size > 1) {
        Left(argument)
      } else if (optionTokens.forall(includeMinorEditKeywords.contains)) {
        Right((sizeTokens.headOption.map(_.toInt), optionTokens.nonEmpty))
      } else {
        Left(argument)
      }
    }
  }

  override def toHtmlString(argument:String)(implicit wikiContext: ContextWikiPage): String = {
    def desc[T : Ordering]: Ordering[T] = implicitly[Ordering[T]].reverse
    parseArgument(argument) match {
      case Left(_) => MacroError.toHtmlString(s"Bad argument - [[$name($argument)]]")
      case Right((sizeOpt, includeMinorEdit)) =>
        val filtered = wikiContext.seqPageByPermission.filter(p => includeMinorEdit || !p.isMinorEdit).sortBy(_.dateTime)(desc)
        toHtmlString(sizeOpt.map(filtered.take).getOrElse(filtered))
    }
  }

  def toHtmlString(list: Seq[PageWithoutContentWithSize])(implicit wikiContext: ContextWikiPage): String = {
    InterpreterWiki.toHtmlString(list.map(p => s""" * ${p.toIsoLocalDateTimeString} - [[Html(<a rel="nofollow" href="/w/${p.name}?action=diff&after=${p.revision}">r${p.revision}</a>)]] - ["${p.name}"] - ${if (p.isMinorEdit) "[minor] " else ""}${p.comment} by ${p.nickname.map(UserPageLogic.wikiMarkup).getOrElse(IpAddressUtil.mask(p.remoteAddress))}""").mkString("\n"))
  }
}
