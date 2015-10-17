package models

import implicits.Implicits._

case class PageContent(s: String) {
  private val split: Array[String] = s.trim.split("""(\n|\r\n)+""")

  val directives = split.toIterator.takeWhile(_.startsWith("#!")).map(_.substring(2)).toSeq
  val content = split.toIterator.dropWhile(_.startsWith("#!")).mkString("\n")

  val read = extractDirective("read")
  val write = extractDirective("write")
  val redirect = extractDirective("redirect")

  def extractDirective(shebang:String): Option[String] = {
    directives.find(_.startsWith(shebang)).map(_.substring(shebang.length).trim)
  }

  val shebang = directives
    .filterNot(_.startsWith("read"))
    .filterNot(_.startsWith("write"))
    .filterNot(_.startsWith("redirect")).flatMap(_.split("""\s+"""))

  val interpreter = shebang.headOption
  val argument = shebang.tailSafe()


}

