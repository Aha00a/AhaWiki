package logics.wikis.interpreters
import models.{PageContent, ContextWikiPage}
import scala.util.matching.Regex

object InterpreterMarkdown extends TraitInterpreter {

  import models.tables.CalculatedLink

  private val regexMarkdownInlineLink: Regex = """(?<!\!)\[([^\]]*)\]\(([^)]*)\)""".r
  private val regexMarkdownAutoLink: Regex = """<([a-zA-Z][a-zA-Z0-9+.-]*://[^>\s]+)>""".r

  override def toHtmlString(content: String)(implicit wikiContext: ContextWikiPage): String = {
    val pageContent: PageContent = PageContent(content)
    com.github.rjeschke.txtmark.Processor.process(pageContent.content)
  }

  override def toSeqLink(content: String)(implicit wikiContext: ContextWikiPage): Seq[CalculatedLink] = {
    val inlineLinks = regexMarkdownInlineLink
      .findAllMatchIn(content)
      .flatMap { m =>
        parseDestination(m.group(2)).map(dst => CalculatedLink(wikiContext.name, dst, m.group(1)))
      }
      .toSeq

    val autoLinks = regexMarkdownAutoLink
      .findAllMatchIn(content)
      .map(m => CalculatedLink(wikiContext.name, m.group(1), ""))
      .toSeq

    inlineLinks ++ autoLinks
  }

  private def parseDestination(raw: String): Option[String] = {
    val trimmed = Option(raw).getOrElse("").trim
    if (trimmed.isEmpty) {
      None
    } else if (trimmed.startsWith("<")) {
      val index = trimmed.indexOf('>')
      if (index > 1) {
        Some(trimmed.substring(1, index))
      } else {
        None
      }
    } else {
      Some(trimmed.takeWhile(!_.isWhitespace))
    }
  }
}
