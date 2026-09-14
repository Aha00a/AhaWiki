package logics.wikis

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

  def notFound(name: String): String = {
    val safeName = asLiteralText(name)
    s"""= $safeName
       |This page does not exist.
       |== Possible actions
       | * [[Html(<a href="?action=edit" rel="nofollow">create page</a>)]]
       | * Search ["https://google.com/search?q=$safeName" $safeName] on Google
       | * Search ["https://google.com/search?q=$safeName wiki" $safeName wiki] on Google
       | * Search ["https://duckduckgo.com/?q=$safeName" $safeName] on DuckDuckGo
       | * Search ["https://duckduckgo.com/?q=$safeName wiki" $safeName wiki] on DuckDuckGo
       |""".stripMargin
  }

  def notFoundWithDayHeader(name: String): String = "[[DayHeader]]\n" + notFound(name)
}
