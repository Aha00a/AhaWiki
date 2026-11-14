package logics

import models.ContextSite

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
 * Used by the page frame (FrameText), the edit screen (EditScreen) and its formatting help
 * (EditHelp).
 */
object ScreenLanguage {

  sealed trait Language { def code: String }
  object Language {
    case object Korean extends Language { val code = "ko" }
    case object English extends Language { val code = "en" }

    /** Korean for a Korean browser, English for every other. The wiki's content is Korean, but
      * someone whose browser asks for another language reads English more readily than Korean. */
    def of(locale: Locale): Language = if (locale.getLanguage == "ko") Korean else English

    def current(implicit context: ContextSite): Language = of(context.requestWrapper.locale)
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

  /**
   * A screen's words by key. A key the table lacks is a mistake in the caller, so `apply` throws
   * rather than print nothing; the render specs reach every template's keys, and the node tests
   * check the scripts' keys from the sources.
   */
  class TextTable(name: String, val texts: Map[String, Text]) {
    def apply(key: String, language: Language): String =
      texts.getOrElse(key, throw new NoSuchElementException(s"$name has no text for '$key'"))(language)

    /** The chosen language's half of the keys a script asks for. */
    def scriptTexts(language: Language, keys: String => Boolean): Map[String, String] =
      texts.collect { case (key, text) if keys(key) => key -> text(language) }
  }
}
