package com.aha00a.tests.unit

import com.aha00a.tests.TestUtil
import logics.EditHelp
import models.ContextWikiPage
import play.api.mvc.{AnyContent, Request}

// The editor's Wiki Formatting Help draws each example with the renderer (EditHelp.scala). An
// example whose markup the renderer does not read the way the label says -- a list without its
// leading space, a link form that went out of use -- would draw as plain text and teach the wrong
// thing. Each label names the element its result has to contain.
object EditHelpUnit {
  def run(testUtil: TestUtil)(implicit request: Request[AnyContent], contextWikiPage: ContextWikiPage): Unit = {
    import testUtil.assertEquals

    val expected: Map[String, Seq[String]] = Map(
      "Bold, italic" -> Seq("<b>bold</b>", "<i>italic</i>"),
      "Code" -> Seq("<code>code</code>"),
      "Link" -> Seq("href=\"/w/FrontPage\""),
      "Link with a label" -> Seq("href=\"/w/FrontPage\"", ">Home</a>"),
      "Heading" -> Seq("<h2"),
      "List" -> Seq("<ul", "<li>item"),
      // InterpreterWiki draws every list as <ul> and numbers it with list-style-type.
      "Numbered list" -> Seq("decimal", "<li>first"),
      "Checkbox" -> Seq("type=\"checkbox\" disabled>", "checked"),
      "Table" -> Seq("<table", "Kim"),
      "Code block" -> Seq("print"),
      "Horizontal line" -> Seq("<hr"),
    )

    val rows = EditHelp.rows
    assertEquals(rows.map(_.example.label).toSet, expected.keySet)
    for (row <- rows; part <- expected(row.example.label)) {
      assertEquals(s"${row.example.label} draws $part: ${row.html.contains(part)}", s"${row.example.label} draws $part: true")
    }
    for (row <- rows) {
      assertEquals(s"${row.example.label} has no error box: ${!row.html.contains("class=\"error\"")}", s"${row.example.label} has no error box: true")
    }
  }
}
