package logics.wikis.macros

import logics.wikis.ExtractConvertInjectMacro
import models.ContextWikiPage
import org.scalatest.freespec.AnyFreeSpec

class MacroKbdSpec extends AnyFreeSpec {
  private implicit val dummyContext: ContextWikiPage = null

  "toHtmlString" - {
    "renders one key" in {
      val html = MacroKbd.toHtmlString("Esc")

      assert(html === """<kbd class="MacroKbd"><kbd>Esc</kbd></kbd>""")
    }

    "renders whitespace-separated key chords" in {
      val html = MacroKbd.toHtmlString("Ctrl Shift P")

      assert(html === """<kbd class="MacroKbd"><kbd>Ctrl</kbd><kbd>Shift</kbd><kbd>P</kbd></kbd>""")
    }

    "collapses repeated whitespace separators" in {
      val html = MacroKbd.toHtmlString("Alt   W")

      assert(html === """<kbd class="MacroKbd"><kbd>Alt</kbd><kbd>W</kbd></kbd>""")
    }

    "renders plus key" in {
      val html = MacroKbd.toHtmlString("Ctrl +")

      assert(html === """<kbd class="MacroKbd"><kbd>Ctrl</kbd><kbd>+</kbd></kbd>""")
    }

    "renders comma key" in {
      val html = MacroKbd.toHtmlString(",")

      assert(html === """<kbd class="MacroKbd"><kbd>,</kbd></kbd>""")
    }

    "escapes html in key labels" in {
      val html = MacroKbd.toHtmlString("<script>")

      assert(html === """<kbd class="MacroKbd"><kbd>&lt;script&gt;</kbd></kbd>""")
    }

    "is registered as a wiki macro" in {
      val converter = new ExtractConvertInjectMacro
      val extracted = converter.extract("[[Kbd(Esc)]]")

      assert(converter.inject(extracted) === """<span class="Macro InlineMacro Kbd"><kbd class="MacroKbd"><kbd>Esc</kbd></kbd></span>""")
    }
  }
}
