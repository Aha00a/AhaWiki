package com.aha00a.tests.unit

import com.aha00a.tests.TestUtil
import logics.wikis.interpreters.InterpreterWiki
import models.ContextWikiPage

object InterpreterWikiUnit {
  def run(testUtil: TestUtil)(implicit contextWikiPage: ContextWikiPage): Unit = {
    val html = InterpreterWiki.toHtmlString("= Title = #custom-id .hero .compact")
    assert(html.contains("""<div class="custom-id">"""))
    assert(html.contains("""<h1 id="custom-id" class="hero compact">"""))

    val htmlColumns = InterpreterWiki.toHtmlString("""<Columns count=\"3\" gap=\"16\" minWidth=\"220\">\n 1. a\n 1. b\n 1. c\n</Columns>""")

    val htmlDiv = InterpreterWiki.toHtmlString("""<div id=\"box\" class=\"card\" style=\"color:red\" onclick=\"evil()\">\n 1. [FrontPage]\n</div>""")
  }
}
