package logics.wikis.macros

import logics.wikis.ExtractConvertInjectMacro
import org.scalatest.freespec.AnyFreeSpec

class MacroThemesSpec extends AnyFreeSpec {
  "render" - {
    "renders both AhaWiki theme previews and token rows" in {
      val html = MacroThemes.render()

      assert(html.contains("""<div class="MacroThemes">"""))
      assert(html.contains("AhaWiki Light"))
      assert(html.contains("AhaWiki Dark"))
      assert(html.contains("--color-bg"))
      assert(html.contains("#ffffff"))
      assert(html.contains("#0f1115"))
      assert(html.contains("Body contrast 21.0:1"))
      assert(html.contains("""class="MacroThemesTable wikiTableSimple""""))
    }
  }

  "contrastRatio" - {
    "calculates WCAG contrast for hex colors" in {
      assert(MacroThemes.contrastRatio("#000", "#fff").exists(ratio => math.abs(ratio - 21.0) < 0.01))
      assert(MacroThemes.contrastRatio("#e6e8eb", "#0f1115").exists(_ > 14.0))
    }
  }

  "registration" - {
    "is registered as a block wiki macro" in {
      assert(ExtractConvertInjectMacro.macroNames.contains("Themes"))
      assert(MacroThemes.isBlock)
    }
  }
}
