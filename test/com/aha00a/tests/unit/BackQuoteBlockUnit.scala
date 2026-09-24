package com.aha00a.tests.unit

import com.aha00a.tests.TestUtil
import logics.wikis.ExtractConvertInjectBackQuote
import logics.wikis.interpreters.Interpreters
import models.ContextWikiPage
import play.api.mvc.{AnyContent, Request}

// Backticks are pulled out of the document before [[[blocks]]] are, so a backtick may protect a
// [[[ or {{var}} written inside it. Before 2026-09-15 that reach went into every block, so a
// backtick inside a #!Text / #!Vim / WikiSyntaxPreview-Raw block came out as a code span instead of
// the literal backtick the block should show, and a Vim block's cache key (md5 of its body) changed
// every render because the body held a fresh placeholder. The fix leaves a LITERAL block's interior
// alone (Text, Vim, WikiSyntaxPreview, and a bare [[[...]]] which defaults to Text). Wiki-like blocks
// (#!Quote, #!Table, ...) re-render their body and turn a backtick into a code span either way, so
// they are left exactly as before -- pulling the backtick here, not in the nested render, keeps their
// <p> wrapping and line breaks unchanged. A [[[ written inside a backtick is still protected.
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

    // The fix: a backtick INSIDE a literal block is left exactly as written.
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
    {
      val e = new ExtractConvertInjectBackQuote()
      val input = "[[[#!WikiSyntaxPreview\n`code`\n]]]"
      assertEquals(e.extract(input), input)
      assertEquals(e.arrayBuffer.isEmpty, true)
    }
    // A bare [[[...]]] defaults to Text, so its interior is literal too.
    {
      val e = new ExtractConvertInjectBackQuote()
      val input = "[[[\n`code`\n]]]"
      assertEquals(e.extract(input), input)
      assertEquals(e.arrayBuffer.isEmpty, true)
    }
    // 2026-09-25: the same has to hold for a block that hands its body on as source rather than
    // showing it. Kanban is the one that was losing text: the board reads its source out of the
    // rendered <pre> with textContent, which drops the <code> tags a pulled backtick left behind,
    // so the next board save wrote the page back with the backticks gone.
    {
      val e = new ExtractConvertInjectBackQuote()
      val input = "[[[#!Kanban\n=== ToDo\n==== card ==== #c1\n===== Description\nuse `Delete(32)`\n]]]"
      assertEquals(e.extract(input), input)
      assertEquals(e.arrayBuffer.isEmpty, true)
    }
    {
      val e = new ExtractConvertInjectBackQuote()
      val input = "[[[#!Mermaid\ngraph TD;\n  A[`tick`]-->B;\n]]]"
      assertEquals(e.extract(input), input)
      assertEquals(e.arrayBuffer.isEmpty, true)
    }
    {
      val e = new ExtractConvertInjectBackQuote()
      val input = "[[[#!Math\na = `b`\n]]]"
      assertEquals(e.extract(input), input)
      assertEquals(e.arrayBuffer.isEmpty, true)
    }
    {
      val e = new ExtractConvertInjectBackQuote()
      val input = "[[[#!Gantt\ntitle\tstart\tend\n`task`\t2026-01-01\t2026-01-02\n]]]"
      assertEquals(e.extract(input), input)
      assertEquals(e.arrayBuffer.isEmpty, true)
    }
    {
      val e = new ExtractConvertInjectBackQuote()
      val input = "[[[#!Html\n<p>a `b` c</p>\n]]]"
      assertEquals(e.extract(input), input)
      assertEquals(e.arrayBuffer.isEmpty, true)
    }

    // A #!read directive before the interpreter is skipped; #!Vim still reads as literal.
    {
      val e = new ExtractConvertInjectBackQuote()
      val input = "[[[#!read all\n#!Vim\n`x`\n]]]"
      assertEquals(e.extract(input), input)
      assertEquals(e.arrayBuffer.isEmpty, true)
    }

    // Scope guard: a wiki-like block keeps the old behavior -- its backtick is pulled here (so the
    // block body carries a placeholder), not left literal. This is what keeps #!Quote/#!Table code
    // cells and lines wrapped and separated exactly as before the fix.
    {
      val e = new ExtractConvertInjectBackQuote()
      val out = e.extract("[[[#!Quote\n`code`\n]]]")
      assertEquals(out.contains("[[[#!Quote"), true)
      assertEquals(out.contains("`code`"), false)
      assertEquals(values(e), List("<code>code</code>"))
    }
    {
      val e = new ExtractConvertInjectBackQuote()
      val out = e.extract("[[[#!Table tsv\na\t`b`\n]]]")
      assertEquals(out.contains("[[[#!Table"), true)
      assertEquals(out.contains("`b`"), false)
      assertEquals(values(e), List("<code>b</code>"))
    }

    // Mixed: backticks outside a literal block are pulled, the block interior is untouched.
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

    // Double backtick (copyable) outside a block is pulled; inside a literal block it stays literal.
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

    // Markdown is deliberately NOT in the set: txtmark makes the same code span out of what it is
    // handed, so leaving it alone keeps one behaviour rather than two that agree.
    {
      val e = new ExtractConvertInjectBackQuote()
      val out = e.extract("[[[#!Markdown\nuse `code` here\n]]]")
      assertEquals(out.contains("`code`"), false)
      assertEquals(values(e), List("<code>code</code>"))
    }

    // End to end: a #!Text block shows the literal backtick, not a code span.
    assertEquals(Interpreters.toHtmlString("[[[#!Text\n`code`\n]]]").contains("`code`"), true)
    assertEquals(Interpreters.toHtmlString("[[[#!Text\n`code`\n]]]").contains("<code>code</code>"), false)
    // End to end: what a Mermaid block hands to the browser is the diagram source, backtick and all.
    assertEquals(Interpreters.toHtmlString("[[[#!Mermaid\nA[`x`]\n]]]").contains("A[`x`]"), true)
    // End to end: the Kanban source the board parses still holds the backtick the card was written
    // with. This is the round trip -- what the board reads here is what it saves back.
    val kanbanHtml = Interpreters.toHtmlString("[[[#!Kanban\n=== ToDo\n==== card ==== #c1\n===== Description\nuse `Delete(32)`\n]]]")
    assertEquals(kanbanHtml.contains("use `Delete(32)`"), true)
    assertEquals(kanbanHtml.contains("<code>Delete(32)</code>"), false)
    // A wiki-like block still turns the backtick into a code span (unchanged behavior).
    assertEquals(Interpreters.toHtmlString("[[[#!Quote\n`code`\n]]]").contains("<code>code</code>"), true)
    // Outside a block a backtick still becomes a code span.
    assertEquals(Interpreters.toHtmlString("`code`").contains("<code>code</code>"), true)
  }
}
