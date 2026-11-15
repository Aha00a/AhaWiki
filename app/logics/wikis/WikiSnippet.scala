package logics.wikis

import com.aha00a.commons.Implicits._
import logics.FrameText
import logics.ScreenLanguage.Language

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
  def asLiteralText(name: String): String =
    name.replace("[", "(").replace("]", ")").replace("`", "'")

  /**
   * One line per template the reader may use, each linking to the editor on this page with that
   * template's text. Each starts with its own newline, so that no template means no blank line to
   * split the list. The name goes through `PageNameUrl.encode` for the address and is escaped for
   * the label, and `)` is spelt out because the macro's argument ends at the first one.
   */
  private def templateLinks(templates: Seq[String], language: Language): String =
    templates.map { template =>
      val href = s"?action=edit&amp;template=${PageNameUrl.encode(template)}".replace(")", "%29")
      val label = PageTemplate.label(template).escapeHtml().replace(")", "&#41;")
      s"""\n * [[Html(<a href="$href" rel="nofollow">${FrameText("notFound.fromTemplate", language)} $label</a>)]]"""
    }.mkString

  /**
   * The page for a name nobody has written, in the reader's language (FrameText). The texts go
   * into this markup as they are, so FrameText keeps them free of the characters that would open
   * a link or close the Html macro.
   */
  def notFound(name: String, templates: Seq[String], language: Language): String = {
    val safeName = asLiteralText(name)
    def t(key: String): String = FrameText(key, language)
    def search(url: String, label: String, engine: String): String =
      " * " + t("notFound.search").replace("{link}", s"""["$url" $label]""").replace("{engine}", engine)
    s"""= $safeName
       |${t("notFound.body")}
       |== ${t("notFound.actions")}
       | * [[Html(<a href="?action=edit" rel="nofollow">${t("notFound.create")}</a>)]]${templateLinks(templates, language)}
       |${search(s"https://google.com/search?q=$safeName", safeName, "Google")}
       |${search(s"https://google.com/search?q=$safeName wiki", s"$safeName wiki", "Google")}
       |${search(s"https://duckduckgo.com/?q=$safeName", safeName, "DuckDuckGo")}
       |${search(s"https://duckduckgo.com/?q=$safeName wiki", s"$safeName wiki", "DuckDuckGo")}
       |""".stripMargin
  }

  def notFoundWithDayHeader(name: String, language: Language): String = "[[DayHeader]]\n" + notFound(name, Seq.empty, language)
}
