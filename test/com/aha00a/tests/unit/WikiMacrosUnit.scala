package com.aha00a.tests.unit

import com.aha00a.tests.TestUtil
import logics.wikis.macros.{MacroAttachment, MacroBr, MacroCalendar, MacroColorCode, MacroImage, MacroKbd, MacroMonthName, MacroPercentLinkTitle, MacroUptime, MacroWeekdayName}
import models.ContextWikiPage

object WikiMacrosUnit {
  def run(testUtil: TestUtil)(implicit contextWikiPage: ContextWikiPage): Unit = {
    import testUtil.assertEquals

    val empty = ""
    val dummy = "dummy"
    assertEquals(MacroBr.toHtmlString(empty), "<br/>")
    assertEquals(MacroBr.toHtmlString(dummy), "<br/>")
    assertEquals(MacroBr.toSeqLink(empty), Seq())
    assertEquals(MacroBr.toSeqLink(dummy), Seq())
    assertEquals(MacroCalendar.name, "Calendar")
    assertEquals(MacroWeekdayName.name, "WeekdayName")
    assertEquals(MacroColorCode.toHtmlString(empty), "")
    assertEquals(MacroColorCode.toHtmlString("#fff"), """<span class="MacroColorCode"><span class="MacroColorCodeSwatch"><span style="background: #fff"></span></span><span class="MacroCopyable"><input class="auto resizeInputToContent" value="#fff" readonly="readonly"/><button type="button" aria-label="Copy" onclick="window.AhaWiki.Clipboard.copy(this.previousElementSibling.value);$(this).next().fadeIn();setTimeout(() =&gt; $(this).next().fadeOut(), 1000);"><i class="far fa-copy" aria-hidden="true"></i></button><span class="tooltip" onclick="$(this).fadeOut();">Copied!!</span></span></span>""")
    assertEquals(MacroColorCode.toHtmlString("#zzz"), """<div class="error">Argument Error - [[ColorCode(#zzz)]]</div>""")
    assertEquals(MacroKbd.name, "Kbd")
    assertEquals(MacroKbd.toHtmlString("Alt W"), """<kbd class="MacroKbd"><kbd>Alt</kbd><kbd>W</kbd></kbd>""")
    assertEquals(MacroKbd.toHtmlString(","), """<kbd class="MacroKbd"><kbd>,</kbd></kbd>""")
    assertEquals(MacroKbd.toHtmlString("<script>"), """<kbd class="MacroKbd"><kbd>&lt;script&gt;</kbd></kbd>""")
    assertEquals(MacroImage.toHtmlString(empty), """<img src="/assets/img/attachmentFallback.svg" alt="/assets/img/attachmentFallback.svg" onerror="this.onerror=null;this.src='/assets/img/attachmentFallback.svg';"/>""")
    assertEquals(MacroImage.toHtmlString("https://example.com/a.png"), """<img src="https://example.com/a.png" alt="https://example.com/a.png" onerror="this.onerror=null;this.src='/assets/img/attachmentFallback.svg';"/>""")
    assertEquals(MacroImage.toHtmlString("https://example.com/a.png, 120"), """<img src="https://example.com/a.png" alt="https://example.com/a.png" onerror="this.onerror=null;this.src='/assets/img/attachmentFallback.svg';" style="width: 120px"/>""")
    assertEquals(MacroImage.toHtmlString("https://example.com/a.png, 50%"), """<img src="https://example.com/a.png" alt="https://example.com/a.png" onerror="this.onerror=null;this.src='/assets/img/attachmentFallback.svg';" style="width: 50%"/>""")
    // The eight-digit form: with `#` it used to be rejected, without it the swatch had no `#`.
    val swatch = (color: String) => s"""<span class="MacroColorCodeSwatch"><span style="background: $color"></span></span>"""
    assertEquals(MacroColorCode.toHtmlString("#000000ff").contains(swatch("#000000ff")), true)
    assertEquals(MacroColorCode.toHtmlString("000000ff").contains(swatch("#000000ff")), true)
    // Attachment's width: the unit was appended twice until 2026-09-12 (`200pxpx`).
    assertEquals(MacroAttachment.parseArgument("a.png, 200px"), ("a.png", Some("200px")))
    assertEquals(MacroAttachment.parseArgument("a.png, 50%"), ("a.png", Some("50%")))
    assertEquals(MacroAttachment.parseArgument("a.png, 200"), ("a.png", Some("200px")))
    assertEquals(MacroAttachment.parseArgument("a.png"), ("a.png", None))
    // A full key names its site, and a page signs only its own site's. Until 2026-09-15 any key in
    // the bucket was signed, so a page could show its readers another wiki's attachment.
    assertEquals(MacroAttachment.toHtmlString("Attachment/987654/Page/a.png").contains("not an attachment of this site"), true)
    // An argument these two cannot read is an error box, not an exception that fails the page.
    assertEquals(MacroMonthName.toHtmlString("Tuesday"), """<div class="error">Argument Error - [[MonthName(Tuesday)]]</div>""")
    // Date-shaped is not the same as a date. WeekdayName matched the shape and then LocalDate.parse
    // threw, so [[WeekdayName(2026-13-45)]] answered 500 for the whole page until 2026-09-25.
    assertEquals(MacroWeekdayName.toHtmlString("2026-13-45"), """<div class="error">Argument Error - [[WeekdayName(2026-13-45)]]</div>""")
    assertEquals(MacroWeekdayName.toHtmlString("2026-09-25").nonEmpty, true)
    assertEquals(MacroPercentLinkTitle.toHtmlString("no commas here"), """<div class="error">Argument Error - [[PercentLinkTitle(no commas here)]]</div>""")
    // A macro written without parentheses means the same as one written with them empty. The
    // extractor's argument group is optional, so `[[Kbd]]` used to hand the macro a null and the
    // NullPointerException took the whole page to 500 -- for Image, Attachment, Copyable, Embed
    // and Kbd, the five that read their argument straight away. Rendering goes through the
    // extractor here, which is where the null was.
    val macroExtractor = new logics.wikis.ExtractConvertInjectMacro()
    def renderMacroCall(call: String): String = macroExtractor.convert(call)
    Seq("Image", "Attachment", "Copyable", "Embed", "Kbd").foreach { name =>
      assertEquals(renderMacroCall(s"[[$name]]"), renderMacroCall(s"[[$name()]]"))
    }
    assertEquals(renderMacroCall("[[Kbd]]").contains("""<kbd class="MacroKbd">"""), true)

    // A macro that throws while rendering shows an error box where it stands, so the next macro
    // with a hole like the ones above does not take the page down. Its argument and the exception
    // are text in the box, not markup.
    val failingMacro = new logics.wikis.macros.TraitMacro {
      override val name: String = "Failing"
      override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String =
        throw new IllegalStateException("<boom>")
    }
    val failed = logics.wikis.ExtractConvertInjectMacro.render(failingMacro, "<arg>")
    assertEquals(failed, "<div class=\"error\">[[Failing(&lt;arg&gt;)]] failed - java.lang.IllegalStateException: &lt;boom&gt;</div>")
    assertEquals(logics.wikis.ExtractConvertInjectMacro.render(MacroBr, ""), "<br/>")

    val uptime = MacroUptime.toHtmlString("")
    assertEquals(MacroUptime.name, "Uptime")
    assertEquals(uptime.matches("""\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d+)?Z"""), true)
  }
}
