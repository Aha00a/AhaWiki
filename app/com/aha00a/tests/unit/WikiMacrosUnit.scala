package com.aha00a.tests.unit

import com.aha00a.tests.TestUtil
import logics.wikis.macros.{MacroBr, MacroCalendar, MacroColorCode, MacroUptime, MacroWeekdayName}
import models.ContextWikiPage

object WikiMacrosUnit {
  def run(testUtil: TestUtil)(implicit contextWikiPage: ContextWikiPage): Unit = {
    import testUtil.assertEquals

    val empty = ""
    val dummy = "dummy"
    assertEquals(MacroBr.toHtmlString(empty), "<br/>")
    assertEquals(MacroBr.toHtmlString(dummy), "<br/>")
    assertEquals(MacroBr.extractLink(empty), Seq())
    assertEquals(MacroBr.extractLink(dummy), Seq())
    assertEquals(MacroCalendar.name, "Calendar")
    assertEquals(MacroWeekdayName.name, "WeekdayName")
    assertEquals(MacroColorCode.toHtmlString(empty), "")
    assertEquals(MacroColorCode.toHtmlString("#fff"), """<span class="MacroColorCode"><spen class="preview" style="background-color: #fff"></spen> #fff</span>""")
    assertEquals(MacroColorCode.toHtmlString("#zzz"), """<div class="error">Argument Error - [[ColorCode(#zzz)]]</div>""")
    val uptime = MacroUptime.toHtmlString("")
    assertEquals(MacroUptime.name, "Uptime")
    assertEquals(uptime.matches("""\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d+)?Z"""), true)
  }
}
