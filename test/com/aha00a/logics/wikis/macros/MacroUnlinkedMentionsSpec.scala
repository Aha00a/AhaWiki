package com.aha00a.logics.wikis.macros

import com.aha00a.tests.provider.EmptyContextWikiPage
import logics.wikis.RenderingMode
import logics.wikis.macros.MacroUnlinkedMentions
import models.ContextWikiPage
import org.scalatest.freespec.AnyFreeSpec

/** The button carries the page's name in an attribute, so the name must not be able to leave it. */
class MacroUnlinkedMentionsSpec extends AnyFreeSpec with EmptyContextWikiPage {
  private def on(name: String): ContextWikiPage = new ContextWikiPage(Seq(name), RenderingMode.Normal)

  "draws a button and an empty place for the answer, and asks nothing yet" in {
    val html = MacroUnlinkedMentions.toHtmlString("")(on("GraphGlow"))
    assert(html.contains("""data-name="GraphGlow""""))
    assert(html.contains("""<button type="button" class="unlinkedMentionsButton">"""))
    assert(html.contains("""<div class="unlinkedMentionsResult"></div>"""))
  }

  "a name with a quote or a tag cannot close the attribute" in {
    val html = MacroUnlinkedMentions.toHtmlString("")(on("""A" onclick="x" <b>"""))
    assert(!html.contains("""onclick="x""""))
    assert(!html.contains("<b>"))
  }

  "is registered, so a page can place it by hand" in {
    assert(logics.wikis.ExtractConvertInjectMacro.macroNames.contains("UnlinkedMentions"))
  }
}
