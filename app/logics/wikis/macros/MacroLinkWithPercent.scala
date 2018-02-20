// TODO: delete this file
//package logics.wikis.macros
//
//import models.WikiContext
//
//object MacroLinkWithPercent extends TraitMacro {
//  val regexLinkWithPercent = """(.+),\s*([\d\.]+%)""".r
//
//  override def apply(argument: String)(implicit wikiContext: WikiContext): String = argument match {
//    case regexLinkWithPercent(link, percent) =>
//      s"""<a href="$link">
//         |  <span class="percentTotal">
//         |    <span class="percentBar" style="width:$percent"></span>
//         |    <span class="percentLabel">$percent</span>
//         |  </span>$link</a>""".stripMargin
//    case _ => s"Error($argument)"
//  }
//
//  override def extractLink(argument: String)(implicit wikiContext: WikiContext): Seq[String] = argument match {
//    case regexLinkWithPercent(link, percent) => Seq(argument)
//    case _ => Seq(argument)
//  }
//}
