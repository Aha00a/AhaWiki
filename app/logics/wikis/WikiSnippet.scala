package logics.wikis

import com.aha00a.commons.Implicits._

object WikiSnippet {

  /**
   * A page name made safe to drop into wiki markup as literal text.
   *
   * The not-found snippet below builds AhaMark from the page name, and the name of a page that
   * does not exist is whatever the URL held. `[` opens a link, a macro (`[[Html(...)]]`) or a
   * block (`[[[#!Html ...]]]`), and the HTML macros emit their argument verbatim -- so until
   * 2026-09-15 `[[Html(<b>x</b>)]]` in the URL rendered as live markup on the not-found page, a
   * reflected XSS anyone could trigger with a link. Turning the brackets into parentheses (and a
   * backtick into an apostrophe) means no construct can form; `<` is left for the renderer, which
   * escapes it. A name that really holds a bracket is only ever shown this way on its not-found
   * page, so the cost is cosmetic.
   */
  private def asLiteralText(name: String): String =
    name.replace("[", "(").replace("]", ")").replace("`", "'")

  /**
   * One line per template the reader may use, each linking to the editor on this page with that
   * template's text. Each starts with its own newline, so that no template means no blank line to
   * split the list. The name goes through `PageNameUrl.encode` for the address and is escaped for
   * the label, and `)` is spelt out because the macro's argument ends at the first one.
   */
  private def templateLinks(templates: Seq[String]): String =
    templates.map { template =>
      val href = s"?action=edit&amp;template=${PageNameUrl.encode(template)}".replace(")", "%29")
      val label = PageTemplate.label(template).escapeHtml().replace(")", "&#41;")
      s"""\n * [[Html(<a href="$href" rel="nofollow">create from template: $label</a>)]]"""
    }.mkString

  def notFound(name: String, templates: Seq[String] = Seq.empty): String = {
    val safeName = asLiteralText(name)
    s"""= $safeName
       |This page does not exist.
       |== Possible actions
       | * [[Html(<a href="?action=edit" rel="nofollow">create page</a>)]]${templateLinks(templates)}
       | * Search ["https://google.com/search?q=$safeName" $safeName] on Google
       | * Search ["https://google.com/search?q=$safeName wiki" $safeName wiki] on Google
       | * Search ["https://duckduckgo.com/?q=$safeName" $safeName] on DuckDuckGo
       | * Search ["https://duckduckgo.com/?q=$safeName wiki" $safeName wiki] on DuckDuckGo
       |""".stripMargin
  }

  def notFoundWithDayHeader(name: String): String = "[[DayHeader]]\n" + notFound(name)
}
