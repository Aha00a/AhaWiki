package logics.wikis.macros

import com.aha00a.commons.Implicits.RichString
import models.ContextWikiPage

import scala.util.matching.Regex

object MacroRuby extends TraitMacro {
  val regex: Regex = """^([^,]+),\s*([^,]+)$""".r

  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    argument match {
      case regex(original, ruby) =>
        val escapedOriginal = original.trim.escapeHtml()
        val escapedRuby = ruby.trim.escapeHtml()
        s"<ruby><rb>$escapedOriginal</rb><rp>(</rp><rt>$escapedRuby</rt><rp>)</rp></ruby>"
      case _ => MacroError.toHtmlString(s"Argument Error - [[$name($argument)]]")
    }
  }
}
