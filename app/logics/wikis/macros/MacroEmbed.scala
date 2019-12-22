package logics.wikis.macros

import models.WikiContext

object MacroEmbed extends TraitMacro {
  val regexYouTube = """(?:youtube(?:-nocookie)?\.com\/(?:[^\/\n\s]+\/\S+\/|(?:v|e(?:mbed)?)\/|\S*?[?&]v=)|youtu\.be\/)([a-zA-Z0-9_-]{11})""".r
  override def apply(argument: String)(implicit wikiContext: WikiContext): String = {
    regexYouTube.findFirstMatchIn(argument).map(m => m.group(1)).map(views.html.Wiki.youtube(_).toString())
      .getOrElse(MacroError(s"Argument Error - [[$name($argument)]]"))
  }

  override def extractLink(argument: String)(implicit wikiContext: WikiContext): Seq[String] = Seq(argument)
}
