package logics.wikis.macros

import models.WikiContext

object MacroLinkWithPercent extends TraitMacro {
  val regexLinkWithPercent = """(.+),\s*([\d\.]+%)""".r

  override def apply(argument: String)(implicit wikiContext: WikiContext): String = {
    argument match {
      case regexLinkWithPercent(link, percent) => s"""<a href="$link"><span class="percentTotal"><span class="percentBar" style="width:$percent"></span></span>$link ($percent)</a>"""
      case _ => s"Error($argument)"
    }
  }

  override def extractLink(argument: String)(implicit wikiContext: WikiContext): Seq[String] = Seq(argument)
}
