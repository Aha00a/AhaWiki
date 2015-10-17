package logics.wikis.macros

object MacroLinkWithPercent {
  val regexLinkWithPercent = """(.+),([\d\.]+%)""".r

  def apply(argument: String): String = {
    argument match {
      case regexLinkWithPercent(link, percent) => s"""<a href="$link"><span class="percentTotal"><span class="percentBar" style="width:$percent"></span></span>$link ($percent)</a>"""
      case _ => argument
    }
  }
}
