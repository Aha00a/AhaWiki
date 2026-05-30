package models

import com.aha00a.commons.Implicits._

case class PageContent(raw: String) {
  private val split: Array[String] = raw.replaceAll("""^\n#!""", "#!").split("""(\n|\r\n)""")

  val directives: Seq[String] = split.iterator.takeWhile(_.startsWith("#!")).map(_.substring(2)).toSeq
  val content: String = split.iterator.dropWhile(_.startsWith("#!")).mkString("\n")

  val redirect: Option[String] = extractDirective("redirect")

  def extractDirective(shebang:String): Option[String] = {
    directives.find(_.startsWith(shebang)).map(_.substring(shebang.length).trim)
  }

  // #!var key value  →  Map(key → value)
  // 첫 번째 단어가 key, 공백/탭 이후 나머지 전체가 value → 값에 '=', 공백, 쉼표 등 자유롭게 포함 가능
  private def isVarDirective(d: String): Boolean = d == "var" || d.startsWith("var ") || d.startsWith("var\t")
  val variables: Map[String, String] = directives
    .filter(isVarDirective)
    .flatMap { d =>
      val s = d.substring("var".length).trim
      val idx = s.indexWhere(c => c == ' ' || c == '\t')
      if (idx > 0) Some(s.substring(0, idx) -> s.substring(idx + 1).trim)
      else None
    }
    .toMap

  val shebang: Seq[String] = directives
    .filterNot(_.startsWith("read"))
    .filterNot(_.startsWith("write"))
    .filterNot(_.startsWith("redirect"))
    .filterNot(isVarDirective)
    .flatMap(_.split("""\s+"""))

  val interpreter: Option[String] = shebang.headOption
  val argument: Seq[String] = shebang.tailSafe()
}

