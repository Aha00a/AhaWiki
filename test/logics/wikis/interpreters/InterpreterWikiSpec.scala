package logics.wikis.interpreters

import com.aha00a.tests.provider.EmptyContextWikiPage
import org.scalatest.freespec.AnyFreeSpec

class InterpreterWikiSpec extends AnyFreeSpec with EmptyContextWikiPage {
  "toHtmlString" - {
    "supports heading id and class attributes" in {
      val html = InterpreterWiki.toHtmlString("= Title = #custom-id .hero .compact")

      assert(html.contains("""<div class="custom-id">"""))
      assert(html.contains("""<h1 id="custom-id" class="hero compact">"""))
      assert(html.contains("""href="#custom-id"""))
    }

    "supports compact heading attribute formats" in {
      val html1 = InterpreterWiki.toHtmlString("== AA == #id")
      val html2 = InterpreterWiki.toHtmlString("== AA == #id.class")
      val html3 = InterpreterWiki.toHtmlString("== AA == .class#id")
      val html4 = InterpreterWiki.toHtmlString("== AA == #id.class1.class2")

      assert(html1.contains("""<div class="id">"""))
      assert(html1.contains("""<h2 id="id" class="">"""))

      assert(html2.contains("""<div class="id">"""))
      assert(html2.contains("""<h2 id="id" class="class">"""))

      assert(html3.contains("""<div class="id">"""))
      assert(html3.contains("""<h2 id="id" class="class">"""))

      assert(html4.contains("""<div class="id">"""))
      assert(html4.contains("""<h2 id="id" class="class1 class2">"""))
    }

    "falls back to generated id when only class attributes are provided" in {
      val html = InterpreterWiki.toHtmlString("= Hello World = .hero")

      assert(html.contains("""<div class="Hello-World">"""))
      assert(html.contains("""<h1 id="Hello-World" class="hero">"""))
      assert(html.contains("""href="#Hello-World"""))
    }

    "supports initially-collapsed headings with > syntax" in {
      val html = InterpreterWiki.toHtmlString("==> Collapsed")

      assert(html.contains("""<div class="Collapsed sectionCollapsed">"""))
      assert(html.contains("""<h2 id="Collapsed" class="">"""))
    }

    "supports Columns block for multi-column list rendering" in {
      val html = InterpreterWiki.toHtmlString(
        """<Columns count="3" gap="16" minWidth="220">
          | 1. a
          | 1. b
          | 1. c
          |</Columns>""".stripMargin
      )

      assert(html.contains("""class="WikiColumns""""))
      assert(html.contains("""column-count: 3;"""))
      assert(html.contains("""column-gap: 16px;"""))
      assert(html.contains("""--column-min-width: 220px;"""))
      assert(html.contains("<li>a</li>"))
      assert(html.contains("<li>b</li>"))
      assert(html.contains("<li>c</li>"))
    }

    "renders wiki syntax inside Columns block" in {
      val html = InterpreterWiki.toHtmlString(
        """<Columns count="2">
          | 1. ''italic''
          | 1. [FrontPage]
          |</Columns>""".stripMargin
      )

      assert(html.contains("<i>italic</i>"))
      assert(html.contains("""href="/w/FrontPage""""))
    }

    "supports div block with only id class style attributes and wiki syntax inside" in {
      val html = InterpreterWiki.toHtmlString(
        """<div id="box" class="card" style="color:red" onclick="evil()">
          | 1. [FrontPage]
          |</div>""".stripMargin
      )

      assert(html.contains("""<div id="box" class="card" style="color:red">"""))
      assert(!html.contains("onclick="))
      assert(html.contains("""href="/w/FrontPage""""))
    }

    "supports span block with wiki inline syntax and filtered attributes" in {
      val html = InterpreterWiki.toHtmlString(
        """<span id="s1" class="muted" style="font-weight:bold" data-x="1">
          | ''text''
          |</span>""".stripMargin
      )

      assert(html.contains("""<span id="s1" class="muted" style="font-weight:bold">"""))
      assert(!html.contains("data-x="))
      assert(html.contains("<i>text</i>"))
    }

    "keeps heading numbering sequence across nested allowed blocks" in {
      val html = InterpreterWiki.toHtmlString(
        """= Outer =
          |<Columns count="2">
          |= Inner =
          |</Columns>""".stripMargin
      )

      assert(html.contains("""<h1 id="Outer""""))
      assert(html.contains("""<h1 id="Inner""""))
      assert(html.contains("""class="headingNumber">1</a>"""))
      assert(html.contains("""class="headingNumber">2</a>"""))
    }

    "includes headings inside Columns in TOC and keeps columns wrapper" in {
      val html = InterpreterWiki.toHtmlString(
        """= Root =
          |== A ==
          |== B ==
          |== C ==
          |== D ==
          |== E ==
          |== F ==
          |<Columns minWidth="820">
          |== Inside ==
          |</Columns>""".stripMargin
      )

      assert(html.contains("""class="WikiColumns""""))
      assert(html.contains("""href="#Inside""""))
    }

    "preserves section edit line numbers for headings inside Columns" in {
      val html = InterpreterWiki.toHtmlString(
        """= Root =
          |<Columns count="2">
          |== Inside ==
          |</Columns>""".stripMargin
      )

      assert(html.contains("""<h2 id="Inside""""))
      assert(html.contains("""data-line-start="3""""))
    }

    "preserves section edit line numbers for headings inside div block" in {
      val html = InterpreterWiki.toHtmlString(
        """= Root =
          |<div class="box">
          |== Inside Div ==
          |</div>""".stripMargin
      )

      assert(html.contains("""<h2 id="Inside-Div""""))
      assert(html.contains("""data-line-start="3""""))
    }

    "resets render state between toHtmlString calls on same context" in {
      val html1 = InterpreterWiki.toHtmlString("= First =")
      val html2 = InterpreterWiki.toHtmlString("= Second =")

      assert(html1.contains("""class="headingNumber">1</a>"""))
      assert(html2.contains("""class="headingNumber">1</a>"""))
    }

    "merges headings from div and Columns blocks into top-level TOC" in {
      val html = InterpreterWiki.toHtmlString(
        """= Root =
          |== 1 ==
          |<div class="box">
          |=== Div Heading ===
          |</div>
          |== 2 ==
          |== 3 ==
          |== 4 ==
          |== 5 ==
          |== 6 ==
          |<Columns minWidth="820">
          |== Col 1 ==
          |== Col 2 ==
          |</Columns>""".stripMargin
      )

      assert(html.contains("""href="#Div-Heading""""))
      assert(html.contains("""href="#Col-1""""))
      assert(html.contains("""href="#Col-2""""))
    }

    "keeps section-edit line numbers correct with multiple tag blocks" in {
      val html = InterpreterWiki.toHtmlString(
        """= Root =
          |== 1 ==
          |<div style="border: 1px solid #f00">
          |=== aa
          |</div>
          |<Columns minWidth="820">
          |== 8 ==
          |</Columns>
          |== 16 ==""".stripMargin
      )

      assert(html.contains("""<h3 id="aa""""))
      assert(html.contains("""data-line-start="4""""))
      assert(html.contains("""<h2 id="8""""))
      assert(html.contains("""data-line-start="7""""))
    }

    "preserves section-edit line end for heading inside div block" in {
      val html = InterpreterWiki.toHtmlString(
        """= Root =
          |== 1 ==
          |<div style="border: 1px solid #f00">
          |111111111
          |=== 1aaa
          |1aaaa
          |</div>
          |== 2 ==""".stripMargin
      )

      assert(html.contains("""<h3 id="1aaa""""))
      assert(html.contains("""data-line-start="5""""))
      assert(html.contains("""data-line-end="8""""))
    }

    "regression: large mixed div and Columns document keeps heading and section-edit ranges stable" in {
      val html = InterpreterWiki.toHtmlString(
        """= Aha00sssa
          |== 1
          |<div style="border: 1px solid #f00">
          |111111111
          |=== 1aaa
          |1aaaa
          |</div>
          |== 2
          |2222
          |2222
          |== 3
          |3333
          |3333
          |== 4
          |4444
          |4444
          |== 5
          |5555
          |== 6
          |6666
          |<Columns minWidth="820">
          |6666
          |6666
          |== 7
          |7777
          |7777
          |== 8
          |8888
          |== 9
          |9999
          |== 10
          |10101010
          |10101010
          |== 11
          |11111111
          |11111111
          |== 12
          |12121212
          |12121212
          |== 13
          |13131313
          |13131313
          |== 14
          |14141414
          |14141414
          |== 15
          |15151515
          |15151515
          |</Columns>
          |== 16
          |16161616
          |16161616
          |== 17
          |17171717
          |17171717""".stripMargin
      )

      assert(!html.contains("""class="headingNumber">0."""))
      assert(html.contains("""href="#1aaa""""))
      assert(html.contains("""href="#7""""))
      assert(html.contains("""href="#17""""))
      assert(html.contains("""<h3 id="1aaa""""))
      assert(html.contains("""data-line-start="5""""))
      assert(html.contains("""data-line-end="8""""))
      assert(html.contains("""<h2 id="7""""))
      assert(html.contains("""data-line-start="24""""))
    }

  }
}
