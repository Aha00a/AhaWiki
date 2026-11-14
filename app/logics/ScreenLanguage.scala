package logics

import models.ContextWikiPage

import java.util.Locale

/**
 * The language a screen speaks to its reader: Korean when the browser asks for Korean, English
 * otherwise.
 *
 * The engine has no translation layer (`conf/messages` is empty and nothing calls Messages). The
 * one thing that already followed the reader was the names of days and months -- MacroWeekdayName,
 * MacroMonthName and the like read `RequestWrapper.locale`, the request's first Accept-Language --
 * so a screen that speaks two languages reads the same, rather than adding a second way to choose.
 * The page's content stays in the site's language (`AhaWikiConfig.ContentLanguage`); this is only
 * the words around it.
 *
 * Used by the edit screen (EditScreen) and its formatting help (EditHelp).
 */
object ScreenLanguage {

  sealed trait Language { def code: String }
  object Language {
    case object Korean extends Language { val code = "ko" }
    case object English extends Language { val code = "en" }

    /** Korean for a Korean browser, English for every other. The wiki's content is Korean, but
      * someone whose browser asks for another language reads English more readily than Korean. */
    def of(locale: Locale): Language = if (locale.getLanguage == "ko") Korean else English

    def current(implicit wikiContext: ContextWikiPage): Language = of(wikiContext.requestWrapper.locale)
  }

  /** A text in both languages, side by side, so a sentence and its translation change together. */
  case class Text(ko: String, en: String) {
    def apply(language: Language): String = language match {
      case Language.Korean => ko
      case Language.English => en
    }
    def nonEmpty: Boolean = ko.nonEmpty
    def both: String = s"$ko $en"
  }

  val NoText: Text = Text("", "")
}
