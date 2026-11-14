package com.aha00a.tests.unit

import com.aha00a.tests.TestUtil
import logics.EditHelp
import logics.ScreenLanguage.Language
import models.ContextWikiPage
import play.api.mvc.{AnyContent, Request}

import java.util.Locale

// The editor's formatting help draws each example with the renderer (EditHelp.scala), in the
// reader's language. An example whose markup the renderer does not read the way its row says --
// a list without its leading space, a link form that went out of use -- would draw as plain text
// and teach the wrong thing. Each drawn row's kind names what its result has to contain, in each
// language. A row the browser draws (drawn = false) has nothing to check here but its sentence
// and its page.
object EditHelpUnit {
  def run(testUtil: TestUtil)(implicit request: Request[AnyContent], contextWikiPage: ContextWikiPage): Unit = {
    import testUtil.assertEquals

    val expected: Map[Language, Map[String, Seq[String]]] = Map(
      Language.Korean -> Map(
        "emphasis" -> Seq("<b>굵게</b>", "<i>기울임</i>"),
        "code" -> Seq("<code>코드</code>"),
        "link" -> Seq("href=\"/w/FrontPage\""),
        "heading" -> Seq("<h2", "제목"),
        "list" -> Seq("<ul", "<li>항목"),
        // InterpreterWiki draws every list as <ul> and numbers it with list-style-type.
        "numbered" -> Seq("decimal", "<li>첫째"),
        "checkbox" -> Seq("type=\"checkbox\" disabled>", "checked"),
        "hr" -> Seq("<hr"),
        "table" -> Seq("<table", "김하나"),
        "vim" -> Seq("print"),
        "fold" -> Seq("자세히 보기", "접어 둔 내용"),
        "quote" -> Seq("두 번 묻지 않는다"),
      ),
      Language.English -> Map(
        "emphasis" -> Seq("<b>bold</b>", "<i>italic</i>"),
        "code" -> Seq("<code>code</code>"),
        "link" -> Seq("href=\"/w/FrontPage\""),
        "heading" -> Seq("<h2", "Heading"),
        "list" -> Seq("<ul", "<li>item"),
        "numbered" -> Seq("decimal", "<li>first"),
        "checkbox" -> Seq("type=\"checkbox\" disabled>", "checked"),
        "hr" -> Seq("<hr"),
        "table" -> Seq("<table", "Kim"),
        "vim" -> Seq("print"),
        "fold" -> Seq("Show more", "Folded away"),
        "quote" -> Seq("nobody asks twice"),
      ),
    )

    for ((language, byKind) <- expected; group <- EditHelp.groups; row <- EditHelp.rows(group, language)) {
      val ex = row.example
      val name = s"${language.code} ${ex.label.en}"
      if (ex.drawn) {
        for (part <- byKind(ex.kind))
          assertEquals(s"$name draws $part: ${row.html.contains(part)}", s"$name draws $part: true")
        assertEquals(s"$name has no error box: ${!row.html.contains("class=\"error\"")}", s"$name has no error box: true")
      } else {
        assertEquals(s"$name says what it draws: ${ex.description(language).nonEmpty && ex.page.nonEmpty}", s"$name says what it draws: true")
      }
    }

    // Every text has both sides: a row missing its translation would show an empty cell.
    for (ex <- EditHelp.examples; (what, text) <- Seq("label" -> ex.label, "markup" -> ex.markup, "note" -> ex.note, "description" -> ex.description)) {
      val complete = text.ko.isEmpty == text.en.isEmpty
      assertEquals(s"${ex.label.en} $what has both languages or neither: $complete", s"${ex.label.en} $what has both languages or neither: true")
    }

    // A markup that runs its interpreter must name one the engine has, or the inserted sample
    // draws "Interpreter not found".
    for (ex <- EditHelp.examples; markup <- Seq(ex.markup.ko, ex.markup.en) if markup.startsWith("[[[#!")) {
      val known = logics.wikis.interpreters.Interpreters.getInterpreter(markup.stripPrefix("[[[").stripSuffix("]]]")).isDefined
      assertEquals(s"${ex.label.en} names a known interpreter: $known", s"${ex.label.en} names a known interpreter: true")
    }

    // The language is the browser's: Korean for Korean, English for anything else.
    assertEquals(Language.of(Locale.KOREA), Language.Korean)
    assertEquals(Language.of(Locale.forLanguageTag("ko")), Language.Korean)
    assertEquals(Language.of(Locale.US), Language.English)
    assertEquals(Language.of(Locale.JAPAN), Language.English)
  }
}
