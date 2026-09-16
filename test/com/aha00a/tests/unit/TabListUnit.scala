package com.aha00a.tests.unit

import com.aha00a.tests.TestUtil
import logics.wikis.interpreters.Interpreters
import models.ContextWikiPage
import play.api.mvc.{AnyContent, Request}

// A line indented with tabs but no bullet marker renders as a nested bullet, one level per leading
// tab. Only tabs trigger it -- a space-indented line stays a paragraph as before -- and a
// tab-then-marker line is still an ordinary list item.
object TabListUnit {
  def run(testUtil: TestUtil)(implicit request: Request[AnyContent], contextWikiPage: ContextWikiPage): Unit = {
    import testUtil.assertEquals

    def count(s: String, sub: String): Int = s.split(java.util.regex.Pattern.quote(sub), -1).length - 1

    // Tabs become bullets, nested by tab depth.
    val out = Interpreters.toHtmlString("\ta\n\t\ta1\n\tb")
    assertEquals(out.contains("<li>a</li>"), true)
    assertEquals(out.contains("<li>a1</li>"), true)
    assertEquals(out.contains("<li>b</li>"), true)
    assertEquals(count(out, "<ul"), 2)      // two levels opened
    assertEquals(count(out, "</ul>"), 2)    // and both closed
    // a1 is nested under a: a new <ul> opens between a and a1.
    val idxA = out.indexOf("<li>a</li>")
    val idxUl2 = out.indexOf("<ul", idxA)
    val idxA1 = out.indexOf("<li>a1</li>")
    assertEquals(idxA >= 0 && idxUl2 > idxA && idxA1 > idxUl2, true)

    // A space-indented line is not a bullet -- it stays a paragraph, unchanged.
    assertEquals(Interpreters.toHtmlString(" a").contains("<li>"), false)

    // A tab-then-marker line is still an ordinary list item (handled by regexList first).
    assertEquals(Interpreters.toHtmlString("\t* x").contains("<li>x</li>"), true)

    // A tab line whose content itself holds tabs (e.g. a Gantt/TSV row) becomes one bullet; the
    // inner tabs stay in the text. This is the accepted consequence of the global rule.
    val g = Interpreters.toHtmlString("\tfoo\tbar")
    assertEquals(g.contains("<li>"), true)
    assertEquals(g.contains("foo"), true)

    // A line that is only tabs (no content) is not a bullet.
    assertEquals(Interpreters.toHtmlString("\t\t").contains("<li>"), false)
  }
}
