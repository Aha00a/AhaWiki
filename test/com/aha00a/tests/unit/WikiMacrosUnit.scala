package com.aha00a.tests.unit

import com.aha00a.tests.TestUtil
import logics.wikis.macros.{MacroBr, MacroCalendar, MacroColorCode, MacroImage, MacroKbd, MacroUptime, MacroWeekdayName}
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
    val uptime = MacroUptime.toHtmlString("")
    assertEquals(MacroUptime.name, "Uptime")
    assertEquals(uptime.matches("""\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d+)?Z"""), true)
  }
}
