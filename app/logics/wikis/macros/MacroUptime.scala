package logics.wikis.macros

import models.ContextWikiPage

import java.lang.management.ManagementFactory
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneOffset}

object MacroUptime extends TraitMacro {
  private val outputFormatter: DateTimeFormatter = DateTimeFormatter.ISO_INSTANT

  private lazy val jvmStartedAtUtc: Instant = {
    Instant.ofEpochMilli(ManagementFactory.getRuntimeMXBean.getStartTime)
  }

  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    outputFormatter.format(jvmStartedAtUtc.atOffset(ZoneOffset.UTC))
  }
}
