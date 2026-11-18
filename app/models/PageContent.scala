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

  // `#!class a b` names classes for one slide of a #!Slide deck (PaperSlideCore). On the deck's first
  // slide it sits right under `#!Slide`, among these lines, so it is kept out of the shebang: as an
  // argument it would have become the deck's own class, `class`.
  val classes: Seq[String] = directives.filter(PageContent.isClassDirective).flatMap(PageContent.classNames)

  val shebang: Seq[String] = directives
    .filterNot(_.startsWith("read"))
    .filterNot(_.startsWith("write"))
    .filterNot(_.startsWith("redirect"))
    .filterNot(isVarDirective)
    .filterNot(PageContent.isClassDirective)
    .flatMap(_.split("""\s+"""))

  val interpreter: Option[String] = shebang.headOption
  val argument: Seq[String] = shebang.tailSafe()
}

object PageContent {
  def isClassDirective(directive: String): Boolean =
    directive == "class" || directive.startsWith("class ") || directive.startsWith("class\t")

  // The class names on a `class` directive (without its `#!`). A name is letters, digits, `_` and `-`;
  // anything else is dropped rather than written into a class attribute.
  def classNames(directive: String): Seq[String] =
    directive.stripPrefix("class").trim.split("""\s+""").toSeq.filter(_.matches("""[A-Za-z0-9_-]+"""))
}
