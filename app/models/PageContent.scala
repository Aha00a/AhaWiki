package models

import com.aha00a.commons.Implicits._

case class PageContent(raw: String) {
  private val split: Array[String] = raw.replaceAll("""^\n#!""", "#!").split("""(\n|\r\n)""")

  val directives: Seq[String] = split.iterator.takeWhile(_.startsWith("#!")).map(_.substring(2)).toSeq
  val content: String = split.iterator.dropWhile(_.startsWith("#!")).mkString("\n")

  val read: Option[String] = extractDirective("read")
  val write: Option[String] = extractDirective("write")
  val redirect: Option[String] = extractDirective("redirect")

  def extractDirective(shebang:String): Option[String] = {
    directives.find(_.startsWith(shebang)).map(_.substring(shebang.length).trim)
  }

  val shebang: Seq[String] = directives
    .filterNot(_.startsWith("read"))
    .filterNot(_.startsWith("write"))
    .filterNot(_.startsWith("redirect")).flatMap(_.split("""\s+"""))

  val interpreter: Option[String] = shebang.headOption
  val argument: Seq[String] = shebang.tailSafe()
}

