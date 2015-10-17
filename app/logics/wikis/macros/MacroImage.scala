package logics.wikis.macros

object MacroImage {
  val regexWidth = """(.+),\s*(\d+(px|%)?)$""".r

  def apply(argument: String): String = {
    argument match {
      case regexWidth(url, width, null) => s"""<img src="$url" alt="$url" style="width: ${width}px"/>"""
      case regexWidth(url, width, unit) => s"""<img src="$url" alt="$url" style="width: $width"/>"""
      case _ => s"""<img src="$argument" alt="$argument"/>"""
    }
  }
}
