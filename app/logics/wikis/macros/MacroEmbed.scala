package logics.wikis.macros

object MacroEmbed {
  val regexYouTube = """(?:youtube(?:-nocookie)?\.com\/(?:[^\/\n\s]+\/\S+\/|(?:v|e(?:mbed)?)\/|\S*?[?&]v=)|youtu\.be\/)([a-zA-Z0-9_-]{11})""".r
  def apply(argument: String): String = {
    regexYouTube.findFirstMatchIn(argument).map(m => m.group(1)).map(views.html.Wiki.youtube(_).toString())
      .getOrElse( s"""Failed to embed $argument""")
  }
}
