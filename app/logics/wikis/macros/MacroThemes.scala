package logics.wikis.macros

import com.aha00a.commons.Implicits._
import models.ContextWikiPage

import java.util.Locale

object MacroThemes extends TraitMacro {
  override def isBlock: Boolean = true

  private[macros] case class ThemePalette(name: String, className: String, values: Map[String, String]) {
    def value(key: String): String = values(key)
  }

  private[macros] case class ThemeToken(group: String, key: String, usage: String, preview: Preview = Preview.Background)

  private[macros] sealed trait Preview
  private[macros] object Preview {
    case object Background extends Preview
    case object Shadow extends Preview
    case object TextShadow extends Preview
  }

  private[macros] val light: ThemePalette = ThemePalette(
    name = "AhaWiki Light",
    className = "ahawikiLight",
    values = Map(
      "--color-bg" -> "#ffffff",
      "--color-text" -> "#000000",
      "--color-link" -> "#015ADC",
      "--color-link-visited" -> "#000066",
      "--color-link-hover-bg" -> "#eaf2ff",
      "--color-link-schema" -> "#000033",
      "--color-missing-decoration" -> "rgba(0, 0, 0, 0.3)",
      "--color-missing-marker" -> "rgba(0, 0, 0, 0.5)",
      "--color-surface" -> "#ffffff",
      "--color-surface-alt" -> "#f7f7f7",
      "--color-surface-panel" -> "linear-gradient(135deg, #eef 0%, #eff 100%)",
      "--color-border" -> "#cccccc",
      "--color-border-subtle" -> "#e3e8ef",
      "--color-muted" -> "#666666",
      "--color-muted-strong" -> "#333333",
      "--color-code-bg" -> "#000022",
      "--color-code-text" -> "#cccccc",
      "--color-code-border" -> "#9999cc",
      "--color-code-link" -> "#3399ff",
      "--color-blockquote-bg" -> "#f3f3ff",
      "--color-blockquote-border" -> "#9999cc",
      "--color-heading-border" -> "#cccccc",
      "--color-heading-shadow" -> "1px 1px 2px #99a",
      "--color-shadow-soft" -> "#eeeeee",
      "--color-overlay-bg" -> "rgba(9, 30, 66, 0.5)",
      "--color-navbar-bg" -> "rgba(255, 255, 255, 0.25)",
      "--color-navbar-border" -> "rgba(217, 225, 238, 0.3)",
      "--color-navbar-shadow" -> "rgba(9, 30, 66, 0.12)",
      "--color-navbar-text" -> "#000000",
      "--color-navbar-home" -> "#0b57d0",
      "--color-navbar-menu-bg" -> "#ffffff",
      "--color-navbar-text-shadow" -> "-1px 1px 10px #fff, 0 1px 10px #fff, 1px 0 10px #fff, 0 1px 10px #fff",
      "--color-input-bg" -> "#ffffff",
      "--color-input-border" -> "#d9e1ee",
      "--color-focus-ring" -> "#268bd2",
      "--color-flash-success-text" -> "#007700",
      "--color-flash-success-bg" -> "#ddffdd",
      "--color-flash-success-border" -> "#00aa00",
      "--color-flash-error-text" -> "#770000",
      "--color-flash-error-bg" -> "#ffdddd",
      "--color-flash-error-border" -> "#dd0000",
      "--color-flash-info-text" -> "#777700",
      "--color-flash-info-bg" -> "#ffffdd",
      "--color-flash-info-border" -> "#aaaa00",
      "--color-flash-warning-text" -> "#774400",
      "--color-flash-warning-bg" -> "#fff2e5",
      "--color-flash-warning-border" -> "#bb6600",
      "--color-table-border" -> "#e5e7eb",
      "--color-table-header-bg" -> "rgba(220, 220, 235, 0.5)",
      "--color-table-row-alt-bg" -> "rgba(245, 245, 255, 0.5)",
      "--color-table-row-hover-bg" -> "rgba(235, 235, 245, 0.5)",
      "--color-percent-track" -> "#cccccc",
      "--color-percent-bar" -> "#000000",
      "--color-percent-label" -> "#ffffff",
      "--color-adjacent-placeholder-bg" -> "linear-gradient(135deg, #f8faff 0%, #f1f5ff 100%)",
      "--color-adjacent-placeholder-border" -> "#d8deea",
      "--color-adjacent-placeholder-text" -> "#4e5f82",
      "--color-adjacent-placeholder-icon" -> "#7088bf",
      "--color-adjacent-placeholder-muted" -> "#6e7fa3",
      "--color-adjacent-hud-bg" -> "rgba(255, 255, 255, 0.5)",
      "--color-adjacent-hud-border" -> "#d8deea",
      "--color-adjacent-hud-shadow" -> "rgba(37, 59, 100, 0.15)",
      "--color-adjacent-hud-text" -> "#2e3b52",
      "--color-adjacent-hud-title" -> "#3d4f70",
      "--color-adjacent-hud-action-bg" -> "#ffffff",
      "--color-adjacent-hud-action-text" -> "#2e3b52",
      "--color-adjacent-hud-action-active-bg" -> "#2b5fc7",
      "--color-adjacent-hud-action-active-text" -> "#ffffff",
      "--color-adjacent-dot-root" -> "#213a9a",
      "--color-adjacent-dot-hop1" -> "#3f7bcf",
      "--color-adjacent-dot-hop2" -> "#7ea2df",
      "--color-kanban-bg" -> "linear-gradient(135deg, #f1f6ff 0%, #f7fbff 100%)",
      "--color-kanban-column-bg" -> "#fbfcff",
      "--color-kanban-card-bg" -> "#ffffff",
      "--color-kanban-border" -> "#d7dce2",
      "--color-kanban-muted" -> "#6b778c",
      "--color-kanban-text" -> "#172b4d",
      "--color-kanban-primary" -> "#0c66e4",
      "--color-kanban-shadow" -> "0 6px 18px rgba(9, 30, 66, 0.08)"
    )
  )

  private[macros] val dark: ThemePalette = ThemePalette(
    name = "AhaWiki Dark",
    className = "ahawikiDark",
    values = Map(
      "--color-bg" -> "#0f1115",
      "--color-text" -> "#e6e8eb",
      "--color-link" -> "#6ea8ff",
      "--color-link-visited" -> "#a8b4ff",
      "--color-link-hover-bg" -> "#1f2f46",
      "--color-link-schema" -> "#b7c9ff",
      "--color-missing-decoration" -> "rgba(224, 224, 224, 0.45)",
      "--color-missing-marker" -> "rgba(224, 224, 224, 0.65)",
      "--color-surface" -> "#171a20",
      "--color-surface-alt" -> "#1d2128",
      "--color-surface-panel" -> "linear-gradient(135deg, #171a20 0%, #202630 100%)",
      "--color-border" -> "#3a404a",
      "--color-border-subtle" -> "#2a3038",
      "--color-muted" -> "#b7bec9",
      "--color-muted-strong" -> "#d9dee6",
      "--color-code-bg" -> "#090b0f",
      "--color-code-text" -> "#d2d6dc",
      "--color-code-border" -> "#48505c",
      "--color-code-link" -> "#6ea8ff",
      "--color-blockquote-bg" -> "#161a22",
      "--color-blockquote-border" -> "#48505c",
      "--color-heading-border" -> "#3a404a",
      "--color-heading-shadow" -> "none",
      "--color-shadow-soft" -> "rgba(0, 0, 0, 0.4)",
      "--color-overlay-bg" -> "rgba(0, 0, 0, 0.55)",
      "--color-navbar-bg" -> "rgba(15, 17, 21, 0.74)",
      "--color-navbar-border" -> "rgba(97, 108, 124, 0.3)",
      "--color-navbar-shadow" -> "rgba(0, 0, 0, 0.35)",
      "--color-navbar-text" -> "#e6e8eb",
      "--color-navbar-home" -> "#6ea8ff",
      "--color-navbar-menu-bg" -> "#171a20",
      "--color-navbar-text-shadow" -> "none",
      "--color-input-bg" -> "#171a20",
      "--color-input-border" -> "#3a404a",
      "--color-focus-ring" -> "#6ea8ff",
      "--color-flash-success-text" -> "#9ee6ad",
      "--color-flash-success-bg" -> "#12351e",
      "--color-flash-success-border" -> "#2f8a45",
      "--color-flash-error-text" -> "#f5a3a8",
      "--color-flash-error-bg" -> "#3a1b24",
      "--color-flash-error-border" -> "#b74f5b",
      "--color-flash-info-text" -> "#f5df7a",
      "--color-flash-info-bg" -> "#3a3215",
      "--color-flash-info-border" -> "#b89122",
      "--color-flash-warning-text" -> "#f3c27c",
      "--color-flash-warning-bg" -> "#3b2618",
      "--color-flash-warning-border" -> "#b87924",
      "--color-table-border" -> "#2a3038",
      "--color-table-header-bg" -> "rgba(110, 120, 135, 0.2)",
      "--color-table-row-alt-bg" -> "rgba(255, 255, 255, 0.03)",
      "--color-table-row-hover-bg" -> "rgba(143, 184, 255, 0.12)",
      "--color-percent-track" -> "#2a3038",
      "--color-percent-bar" -> "#8fb8ff",
      "--color-percent-label" -> "#ffffff",
      "--color-adjacent-placeholder-bg" -> "linear-gradient(135deg, #171a20 0%, #0f1115 100%)",
      "--color-adjacent-placeholder-border" -> "#3a404a",
      "--color-adjacent-placeholder-text" -> "#d9dee6",
      "--color-adjacent-placeholder-icon" -> "#9fc1ff",
      "--color-adjacent-placeholder-muted" -> "#b7bec9",
      "--color-adjacent-hud-bg" -> "rgba(23, 26, 32, 0.82)",
      "--color-adjacent-hud-border" -> "#3a404a",
      "--color-adjacent-hud-shadow" -> "rgba(0, 0, 0, 0.35)",
      "--color-adjacent-hud-text" -> "#e6e8eb",
      "--color-adjacent-hud-title" -> "#d9dee6",
      "--color-adjacent-hud-action-bg" -> "#1d2128",
      "--color-adjacent-hud-action-text" -> "#e6e8eb",
      "--color-adjacent-hud-action-active-bg" -> "#8fb8ff",
      "--color-adjacent-hud-action-active-text" -> "#090b0f",
      "--color-adjacent-dot-root" -> "#9fc1ff",
      "--color-adjacent-dot-hop1" -> "#8fb8ff",
      "--color-adjacent-dot-hop2" -> "#b7bec9",
      "--color-kanban-bg" -> "linear-gradient(135deg, #111419 0%, #171a20 100%)",
      "--color-kanban-column-bg" -> "#171a20",
      "--color-kanban-card-bg" -> "#1d2128",
      "--color-kanban-border" -> "#3a404a",
      "--color-kanban-muted" -> "#b7bec9",
      "--color-kanban-text" -> "#e6e8eb",
      "--color-kanban-primary" -> "#8fb8ff",
      "--color-kanban-shadow" -> "0 6px 18px rgba(0, 0, 0, 0.28)"
    )
  )

  private[macros] val tokens: Seq[ThemeToken] = Seq(
    ThemeToken("Base", "--color-bg", "Page background"),
    ThemeToken("Base", "--color-text", "Body text"),
    ThemeToken("Base", "--color-surface", "Primary surface"),
    ThemeToken("Base", "--color-surface-alt", "Secondary surface"),
    ThemeToken("Base", "--color-surface-panel", "Panel surface"),
    ThemeToken("Base", "--color-border", "Default border"),
    ThemeToken("Base", "--color-border-subtle", "Subtle border"),
    ThemeToken("Base", "--color-muted", "Muted text"),
    ThemeToken("Base", "--color-muted-strong", "Strong muted text"),
    ThemeToken("Content", "--color-link", "Link"),
    ThemeToken("Content", "--color-link-visited", "Visited link"),
    ThemeToken("Content", "--color-link-hover-bg", "Link hover background"),
    ThemeToken("Content", "--color-link-schema", "Schema link"),
    ThemeToken("Content", "--color-missing-decoration", "Missing page underline"),
    ThemeToken("Content", "--color-missing-marker", "Missing page marker"),
    ThemeToken("Content", "--color-code-bg", "Code block background"),
    ThemeToken("Content", "--color-code-text", "Code block text"),
    ThemeToken("Content", "--color-code-border", "Code block border"),
    ThemeToken("Content", "--color-code-link", "Code block link"),
    ThemeToken("Content", "--color-blockquote-bg", "Blockquote background"),
    ThemeToken("Content", "--color-blockquote-border", "Blockquote border"),
    ThemeToken("Content", "--color-heading-border", "Heading border"),
    ThemeToken("Content", "--color-heading-shadow", "Heading shadow", Preview.TextShadow),
    ThemeToken("UI", "--color-shadow-soft", "Soft shadow"),
    ThemeToken("UI", "--color-overlay-bg", "Overlay"),
    ThemeToken("UI", "--color-input-bg", "Input background"),
    ThemeToken("UI", "--color-input-border", "Input border"),
    ThemeToken("UI", "--color-focus-ring", "Focus ring"),
    ThemeToken("Navigation", "--color-navbar-bg", "Navbar background"),
    ThemeToken("Navigation", "--color-navbar-border", "Navbar border"),
    ThemeToken("Navigation", "--color-navbar-shadow", "Navbar shadow"),
    ThemeToken("Navigation", "--color-navbar-text", "Navbar text"),
    ThemeToken("Navigation", "--color-navbar-home", "Home link"),
    ThemeToken("Navigation", "--color-navbar-menu-bg", "Dropdown background"),
    ThemeToken("Navigation", "--color-navbar-text-shadow", "Navbar text shadow", Preview.TextShadow),
    ThemeToken("Flash", "--color-flash-success-text", "Success text"),
    ThemeToken("Flash", "--color-flash-success-bg", "Success background"),
    ThemeToken("Flash", "--color-flash-success-border", "Success border"),
    ThemeToken("Flash", "--color-flash-error-text", "Error text"),
    ThemeToken("Flash", "--color-flash-error-bg", "Error background"),
    ThemeToken("Flash", "--color-flash-error-border", "Error border"),
    ThemeToken("Flash", "--color-flash-info-text", "Info text"),
    ThemeToken("Flash", "--color-flash-info-bg", "Info background"),
    ThemeToken("Flash", "--color-flash-info-border", "Info border"),
    ThemeToken("Flash", "--color-flash-warning-text", "Warning text"),
    ThemeToken("Flash", "--color-flash-warning-bg", "Warning background"),
    ThemeToken("Flash", "--color-flash-warning-border", "Warning border"),
    ThemeToken("Tables", "--color-table-border", "Table border"),
    ThemeToken("Tables", "--color-table-header-bg", "Table header"),
    ThemeToken("Tables", "--color-table-row-alt-bg", "Alternating row"),
    ThemeToken("Tables", "--color-table-row-hover-bg", "Hover row"),
    ThemeToken("Metrics", "--color-percent-track", "Percent track"),
    ThemeToken("Metrics", "--color-percent-bar", "Percent bar"),
    ThemeToken("Metrics", "--color-percent-label", "Percent label"),
    ThemeToken("Adjacent Graph", "--color-adjacent-placeholder-bg", "Placeholder background"),
    ThemeToken("Adjacent Graph", "--color-adjacent-placeholder-border", "Placeholder border"),
    ThemeToken("Adjacent Graph", "--color-adjacent-placeholder-text", "Placeholder text"),
    ThemeToken("Adjacent Graph", "--color-adjacent-placeholder-icon", "Placeholder icon"),
    ThemeToken("Adjacent Graph", "--color-adjacent-placeholder-muted", "Placeholder muted text"),
    ThemeToken("Adjacent Graph", "--color-adjacent-hud-bg", "HUD background"),
    ThemeToken("Adjacent Graph", "--color-adjacent-hud-border", "HUD border"),
    ThemeToken("Adjacent Graph", "--color-adjacent-hud-shadow", "HUD shadow"),
    ThemeToken("Adjacent Graph", "--color-adjacent-hud-text", "HUD text"),
    ThemeToken("Adjacent Graph", "--color-adjacent-hud-title", "HUD title"),
    ThemeToken("Adjacent Graph", "--color-adjacent-hud-action-bg", "HUD action"),
    ThemeToken("Adjacent Graph", "--color-adjacent-hud-action-text", "HUD action text"),
    ThemeToken("Adjacent Graph", "--color-adjacent-hud-action-active-bg", "HUD active action"),
    ThemeToken("Adjacent Graph", "--color-adjacent-hud-action-active-text", "HUD active action text"),
    ThemeToken("Adjacent Graph", "--color-adjacent-dot-root", "Root node"),
    ThemeToken("Adjacent Graph", "--color-adjacent-dot-hop1", "1-hop node"),
    ThemeToken("Adjacent Graph", "--color-adjacent-dot-hop2", "2-hop node"),
    ThemeToken("Kanban", "--color-kanban-bg", "Board background"),
    ThemeToken("Kanban", "--color-kanban-column-bg", "Column background"),
    ThemeToken("Kanban", "--color-kanban-card-bg", "Card background"),
    ThemeToken("Kanban", "--color-kanban-border", "Border"),
    ThemeToken("Kanban", "--color-kanban-muted", "Muted text"),
    ThemeToken("Kanban", "--color-kanban-text", "Text"),
    ThemeToken("Kanban", "--color-kanban-primary", "Primary accent"),
    ThemeToken("Kanban", "--color-kanban-shadow", "Card shadow", Preview.Shadow)
  )

  private val themes = Seq(light, dark)

  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String =
    render()

  private[macros] def render(): String = {
    val previews = themes.map(renderPreview).mkString
    val rows = renderTokenRows()

    s"""<div class="MacroThemes">
       |<div class="MacroThemesPreviewGrid">$previews</div>
       |<div class="MacroThemesTableWrapper">
       |<table class="MacroThemesTable wikiTableSimple">
       |<thead><tr><th>Token</th><th>Usage</th><th>AhaWiki Light</th><th>AhaWiki Dark</th></tr></thead>
       |<tbody>$rows</tbody>
       |</table>
       |</div>
       |</div>""".stripMargin.replaceAll("\n", "")
  }

  private def renderPreview(theme: ThemePalette): String = {
    val style = Seq(
      "--theme-preview-bg" -> theme.value("--color-bg"),
      "--theme-preview-text" -> theme.value("--color-text"),
      "--theme-preview-link" -> theme.value("--color-link"),
      "--theme-preview-border" -> theme.value("--color-border"),
      "--theme-preview-surface" -> theme.value("--color-surface"),
      "--theme-preview-code-bg" -> theme.value("--color-code-bg"),
      "--theme-preview-code-text" -> theme.value("--color-code-text"),
      "--theme-preview-code-border" -> theme.value("--color-code-border")
    ).map { case (key, value) => s"$key: ${value.escapeHtmlAttribute()}" }.mkString("; ")

    s"""<section class="MacroThemesPreview ${theme.className.escapeHtmlAttribute()}" style="$style">
       |<div class="MacroThemesPreviewTitle">${theme.name.escapeHtml()}</div>
       |<p>Body text sample with <span class="MacroThemesPreviewLink">link text</span>.</p>
       |<code class="MacroThemesPreviewCode">code sample</code>
       |<div class="MacroThemesContrast">Body contrast ${bodyContrast(theme).escapeHtml()}</div>
       |</section>""".stripMargin.replaceAll("\n", "")
  }

  private def renderTokenRows(): String = {
    tokens
      .groupBy(_.group)
      .toSeq
      .sortBy { case (_, groupedTokens) => tokens.indexOf(groupedTokens.head) }
      .map { case (group, groupedTokens) =>
        val groupRow = s"""<tr class="MacroThemesGroup"><th colspan="4">${group.escapeHtml()}</th></tr>"""
        groupRow + groupedTokens.map(renderTokenRow).mkString
      }
      .mkString
  }

  private def renderTokenRow(token: ThemeToken): String =
    s"""<tr>
       |<th><code>${token.key.escapeHtml()}</code></th>
       |<td>${token.usage.escapeHtml()}</td>
       |${themes.map(theme => renderValueCell(theme.value(token.key), token.preview)).mkString}
       |</tr>""".stripMargin.replaceAll("\n", "")

  private def renderValueCell(value: String, preview: Preview): String =
    s"""<td><span class="MacroThemesValue">${renderSwatch(value, preview)}${MacroCopyable.doToHtmlString(value)}</span></td>"""

  private def renderSwatch(value: String, preview: Preview): String = {
    val escaped = value.escapeHtmlAttribute()
    preview match {
      case Preview.Background =>
        s"""<span class="MacroThemesSwatch"><span style="background: $escaped"></span></span>"""
      case Preview.Shadow =>
        s"""<span class="MacroThemesSwatch MacroThemesSwatchShadow"><span style="box-shadow: $escaped"></span></span>"""
      case Preview.TextShadow =>
        s"""<span class="MacroThemesSwatch MacroThemesSwatchTextShadow"><span style="text-shadow: $escaped">Aa</span></span>"""
    }
  }

  private def bodyContrast(theme: ThemePalette): String =
    contrastRatio(theme.value("--color-text"), theme.value("--color-bg"))
      .map(v => f"$v%.1f:1")
      .getOrElse("n/a")

  private[macros] def contrastRatio(foreground: String, background: String): Option[Double] =
    for {
      fg <- parseHexColor(foreground)
      bg <- parseHexColor(background)
    } yield {
      val lighter = math.max(relativeLuminance(fg), relativeLuminance(bg))
      val darker = math.min(relativeLuminance(fg), relativeLuminance(bg))
      (lighter + 0.05) / (darker + 0.05)
    }

  private def parseHexColor(value: String): Option[(Int, Int, Int)] = {
    val normalized = value.trim.toLowerCase(Locale.ROOT)
    if (normalized.matches("^#[0-9a-f]{3}$")) {
      Some((
        Integer.parseInt(normalized.substring(1, 2) * 2, 16),
        Integer.parseInt(normalized.substring(2, 3) * 2, 16),
        Integer.parseInt(normalized.substring(3, 4) * 2, 16)
      ))
    } else if (normalized.matches("^#[0-9a-f]{6}$")) {
      Some((
        Integer.parseInt(normalized.substring(1, 3), 16),
        Integer.parseInt(normalized.substring(3, 5), 16),
        Integer.parseInt(normalized.substring(5, 7), 16)
      ))
    } else {
      None
    }
  }

  private def relativeLuminance(rgb: (Int, Int, Int)): Double = {
    val (r, g, b) = rgb
    Seq(r, g, b)
      .map { channel =>
        val v = channel / 255.0
        if (v <= 0.03928) v / 12.92 else math.pow((v + 0.055) / 1.055, 2.4)
      }
      .zip(Seq(0.2126, 0.7152, 0.0722))
      .map { case (channel, weight) => channel * weight }
      .sum
  }
}
