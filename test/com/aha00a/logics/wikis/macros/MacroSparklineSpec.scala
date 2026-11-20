package logics.wikis.macros

import logics.wikis.ExtractConvertInjectMacro
import models.ContextWikiPage
import org.scalatest.freespec.AnyFreeSpec

class MacroSparklineSpec extends AnyFreeSpec {
  private implicit val dummyContext: ContextWikiPage = null

  private def points(html: String): Seq[(Double, Double)] =
    """points="([^"]*)"""".r.findFirstMatchIn(html).get.group(1).split(" ").toSeq.map(_.split(",") match {
      case Array(x, y) => (x.toDouble, y.toDouble)
    })

  "toHtmlString" - {
    "draws one point per value, the highest at the top and the lowest at the bottom" in {
      val html = MacroSparkline.toHtmlString("3 5 1")

      assert(points(html) === Seq((2.5, 10.0), (40.0, 2.5), (77.5, 17.5)))
    }

    "puts a dot on the last value" in {
      assert(MacroSparkline.toHtmlString("3 5 1").contains("""<circle cx="77.5" cy="17.5" r="2"/>"""))
    }

    "keeps the values as written for the title and the accessible name" in {
      val html = MacroSparkline.toHtmlString("  1,200   -7%  (3) ")

      assert(html.contains("""aria-label="1,200 -7% (3)""""))
      assert(html.contains("<title>1,200 -7% (3)</title>"))
    }

    "reads each value as a table cell does, so 1,234 is one value" in {
      assert(points(MacroSparkline.toHtmlString("1,234 5,678")).size === 2)
      assert(MacroSparkline.parse(Seq("(12)", "-7%", "1,234")) === Some(Seq(-12.0, -7.0, 1234.0)))
    }

    "draws a flat series across the middle" in {
      assert(points(MacroSparkline.toHtmlString("4 4 4")).map(_._2).distinct === Seq(10.0))
    }

    "writes coordinates with a dot, not the locale's decimal comma" in {
      val saved = java.util.Locale.getDefault
      java.util.Locale.setDefault(java.util.Locale.GERMANY)
      try assert(points(MacroSparkline.toHtmlString("1 2 3 4 5 6 7")).nonEmpty)
      finally java.util.Locale.setDefault(saved)
    }

    "refuses a word that is not a number rather than skipping it" in {
      assert(MacroSparkline.parse(Seq("1", "-", "3")) === None)
      assert(MacroSparkline.toHtmlString("1 x 3").contains("Argument Error - [[Sparkline(1 x 3)]]"))
    }

    "refuses fewer than two values" in {
      assert(MacroSparkline.parse(Seq("5")) === None)
      assert(MacroSparkline.parse(Seq()) === None)
    }

    "escapes the written values in the title" in {
      assert(!MacroSparkline.svg(Seq(1, 2), "1 <b>").contains("<b>"))
    }

    "is registered as a wiki macro" in {
      val converter = new ExtractConvertInjectMacro
      val html = converter.inject(converter.extract("[[Sparkline(1 2)]]"))

      assert(html.startsWith("""<span class="Macro InlineMacro Sparkline"><svg class="MacroSparkline""""))
    }
  }
}
