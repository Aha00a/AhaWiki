package logics.wikis

import java.time.LocalDate

/**
 * Pages named `Template:<Something>` are templates: the text a new page can start from.
 *
 * Nothing about them is special to the wiki otherwise — they are ordinary pages, with the ordinary
 * permissions and history, written in AhaMark. That is the point of keeping them as pages: a
 * template is edited, reviewed and linked like any other, and a wiki that wants none simply has no
 * such page.
 *
 * Creating from one is `?action=edit&template=Template:Person` on the page being created, and the
 * not-found page lists the templates a reader may use.
 */
object PageTemplate {
  val Prefix = "Template:"

  def isTemplate(name: String): Boolean = name.startsWith(Prefix) && name.length > Prefix.length

  /** What the template is called in a list: its name without the prefix. */
  def label(name: String): String = name.stripPrefix(Prefix)

  /**
   * The template's text for a new page called `pageName`.
   *
   * Two tokens are filled in: `{{PageName}}` and `{{Date}}` (an ISO date). They are the only ones,
   * and they are filled once, when the page is created. A `{{name}}` that is not one of them is left
   * alone, because it may be a variable the new page means to use ([Variable]); a template that
   * filled every `{{...}}` would break those silently.
   */
  def fill(template: String, pageName: String, today: LocalDate): String =
    template
      .replace("{{PageName}}", pageName)
      .replace("{{Date}}", today.toString)
}
