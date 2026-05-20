package logics.wikis.macros

import models.ContextWikiPage
import org.scalatest.freespec.AnyFreeSpec

class MacroRubySpec extends AnyFreeSpec {
  private implicit val dummyContext: ContextWikiPage = null

  "toHtmlString" - {
    "renders ruby annotation from comma-separated argument" in {
      val html = MacroRuby.toHtmlString("漢字, かんじ")

      assert(html === "<ruby><rb>漢字</rb><rp>(</rp><rt>かんじ</rt><rp>)</rp></ruby>")
    }

    "escapes html in both original text and ruby text" in {
      val html = MacroRuby.toHtmlString("<b>, <script>")

      assert(html === "<ruby><rb>&lt;b&gt;</rb><rp>(</rp><rt>&lt;script&gt;</rt><rp>)</rp></ruby>")
    }

    "returns argument error when format is invalid" in {
      val html = MacroRuby.toHtmlString("invalid")

      assert(html.contains("Argument Error"))
    }
  }
}
