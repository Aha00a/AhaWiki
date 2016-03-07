package logics.wikis.macros

object MacroLinkWithPercent {
  val regexLinkWithPercent = """(.+),\s*([\d\.]+%)""".r

  def apply(argument: String): String = {
    argument match {
      case regexLinkWithPercent(link, percent) => s"""<a href="$link"><span class="percentTotal"><span class="percentBar" style="width:$percent"></span></span>$link ($percent)</a>"""
      case _ => s"Error($argument)"
    }
  }
}
