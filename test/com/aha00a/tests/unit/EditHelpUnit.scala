package com.aha00a.tests.unit

import com.aha00a.tests.TestUtil
import logics.EditHelp
import models.ContextWikiPage
import play.api.mvc.{AnyContent, Request}

// The editor's formatting help draws each example with the renderer (EditHelp.scala). An
// example whose markup the renderer does not read the way its row says -- a list without its
// leading space, a link form that went out of use -- would draw as plain text and teach the wrong
// thing. Each drawn row's kind names what its result has to contain. A row the browser draws
// (drawn = false) has nothing to check here but its sentence and its page.
object EditHelpUnit {
  def run(testUtil: TestUtil)(implicit request: Request[AnyContent], contextWikiPage: ContextWikiPage): Unit = {
    import testUtil.assertEquals

    val expected: Map[String, Seq[String]] = Map(
      "emphasis" -> Seq("<b>굵게</b>", "<i>기울임</i>"),
      "code" -> Seq("<code>코드</code>"),
      "link" -> Seq("href=\"/w/FrontPage\""),
      "heading" -> Seq("<h2"),
      "list" -> Seq("<ul", "<li>항목"),
      // InterpreterWiki draws every list as <ul> and numbers it with list-style-type.
      "numbered" -> Seq("decimal", "<li>첫째"),
      "checkbox" -> Seq("type=\"checkbox\" disabled>", "checked"),
      "hr" -> Seq("<hr"),
      "table" -> Seq("<table", "김하나"),
      "vim" -> Seq("print"),
      "fold" -> Seq("자세히 보기", "접어 둔 내용"),
      "quote" -> Seq("두 번 묻지 않는다"),
    )

    for (group <- EditHelp.groups; row <- EditHelp.rows(group)) {
      val ex = row.example
      if (ex.drawn) {
        for (part <- expected(ex.kind))
          assertEquals(s"${ex.label} draws $part: ${row.html.contains(part)}", s"${ex.label} draws $part: true")
        assertEquals(s"${ex.label} has no error box: ${!row.html.contains("class=\"error\"")}", s"${ex.label} has no error box: true")
      } else {
        assertEquals(s"${ex.label} says what it draws: ${ex.description.nonEmpty && ex.page.nonEmpty}", s"${ex.label} says what it draws: true")
      }
    }
    // A markup that runs its interpreter must name one the engine has, or the inserted sample
    // draws "Interpreter not found".
    for (ex <- EditHelp.examples if ex.markup.startsWith("[[[#!")) {
      val known = logics.wikis.interpreters.Interpreters.getInterpreter(ex.markup.stripPrefix("[[[").stripSuffix("]]]")).isDefined
      assertEquals(s"${ex.label} names a known interpreter: $known", s"${ex.label} names a known interpreter: true")
    }
    assertEquals(expected.keySet.subsetOf(EditHelp.examples.map(_.kind).toSet), true)
  }
}
