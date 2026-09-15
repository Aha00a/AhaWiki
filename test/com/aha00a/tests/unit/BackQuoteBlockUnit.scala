package com.aha00a.tests.unit

import com.aha00a.tests.TestUtil
import logics.wikis.ExtractConvertInjectBackQuote
import logics.wikis.interpreters.Interpreters
import models.ContextWikiPage
import play.api.mvc.{AnyContent, Request}

// Backticks are pulled out of the document before [[[blocks]]] are, so that a backtick may protect a
// [[[ or {{var}} written inside it. Before 2026-09-15 that reach went into the blocks too, so a
// backtick inside a #!Text / #!Vim / WikiSyntaxPreview-Raw block came out as a code span instead of
// the literal backtick the block should show, and a Vim block's cache key (md5 of its body) changed
// every render because the body held a fresh placeholder. The fix leaves a block's interior alone
// while still pulling backticks everywhere else, and still protecting a [[[ written inside a backtick.
object BackQuoteBlockUnit {
  def run(testUtil: TestUtil)(implicit request: Request[AnyContent], contextWikiPage: ContextWikiPage): Unit = {
    import testUtil.assertEquals

    def values(e: ExtractConvertInjectBackQuote): List[String] = e.arrayBuffer.map(_._2).toList

    // A backtick outside any block is pulled out and stored as its rendered HTML.
    {
      val e = new ExtractConvertInjectBackQuote()
      val out = e.extract("`code`")
      assertEquals(out.contains("`"), false)
      assertEquals(values(e), List("<code>code</code>"))
    }
    {
      val e = new ExtractConvertInjectBackQuote()
      val out = e.extract("a `b` c")
      assertEquals(out.contains("`"), false)
      assertEquals(out.startsWith("a "), true)
      assertEquals(out.endsWith(" c"), true)
      assertEquals(values(e), List("<code>b</code>"))
    }

    // A [[[ written inside a backtick is protected: the backtick wins, the [[[ opens no block.
    {
      val e = new ExtractConvertInjectBackQuote()
      val out = e.extract("`[[[`")
      assertEquals(out.contains("`"), false)
      assertEquals(out.contains("[[["), false)
      assertEquals(values(e), List("<code>[[[</code>"))
    }

    // The fix: a backtick INSIDE a [[[block]]] is left exactly as written.
    {
      val e = new ExtractConvertInjectBackQuote()
      val input = "[[[#!Text\n`code`\n]]]"
      assertEquals(e.extract(input), input)
      assertEquals(e.arrayBuffer.isEmpty, true)
    }
    {
      val e = new ExtractConvertInjectBackQuote()
      val input = "[[[#!Vim\nprintln(`x`)\n]]]"
      assertEquals(e.extract(input), input)
      assertEquals(e.arrayBuffer.isEmpty, true)
    }

    // Mixed: backticks outside the block are pulled, the block interior is untouched.
    {
      val e = new ExtractConvertInjectBackQuote()
      val out = e.extract("`a`\n[[[#!Text\n`b`\n]]]\n`c`")
      assertEquals(out.contains("[[[#!Text\n`b`\n]]]"), true)
      assertEquals(out.contains("`a`"), false)
      assertEquals(out.contains("`c`"), false)
      assertEquals(values(e), List("<code>a</code>", "<code>c</code>"))
    }

    // A [[[ with no closing ]]] is not a block; a backtick after it is handled normally.
    {
      val e = new ExtractConvertInjectBackQuote()
      val out = e.extract("[[[#!Text\n`code`")
      assertEquals(out.contains("`"), false)
      assertEquals(values(e), List("<code>code</code>"))
    }

    // Double backtick (copyable) outside a block is still pulled; inside a block it stays literal.
    {
      val e = new ExtractConvertInjectBackQuote()
      val out = e.extract("``copy``")
      assertEquals(out.contains("`"), false)
      assertEquals(e.arrayBuffer.length, 1)
    }
    {
      val e = new ExtractConvertInjectBackQuote()
      val input = "[[[#!Text\n``copy``\n]]]"
      assertEquals(e.extract(input), input)
      assertEquals(e.arrayBuffer.isEmpty, true)
    }

    // End to end: a #!Text block shows the literal backtick, not a code span.
    assertEquals(Interpreters.toHtmlString("[[[#!Text\n`code`\n]]]").contains("`code`"), true)
    assertEquals(Interpreters.toHtmlString("[[[#!Text\n`code`\n]]]").contains("<code>code</code>"), false)
    // Outside a block a backtick still becomes a code span.
    assertEquals(Interpreters.toHtmlString("`code`").contains("<code>code</code>"), true)
  }
}
