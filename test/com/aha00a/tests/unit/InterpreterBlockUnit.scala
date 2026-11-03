package com.aha00a.tests.unit

import com.aha00a.tests.TestUtil
import logics.wikis.interpreters.{InterpreterSchema, InterpreterWiki, Interpreters}
import logics.wikis.interpreters.ahaMark.AhaMarkLink
import models.ContextWikiPage
import play.api.mvc.{AnyContent, Request}

object InterpreterBlockUnit {
  def run(testUtil: TestUtil)(implicit request: Request[AnyContent], contextWikiPage: ContextWikiPage): Unit = {
    import testUtil.assertEquals

    assertEquals(Interpreters.getInterpreter("#!Vim scala\nprintln(1)").map(_.name), Some("Vim"))
    assertEquals(Interpreters.getInterpreter("#!read all\n#!write aha00a\n#!Vim scala\nprintln(1)").map(_.name), Some("Vim"))
    assertEquals(Interpreters.getInterpreter("plain text").map(_.name), Some("Wiki"))

    // A block that throws while rendering shows an error box where it stands; the page around it
    // lives. Until 2026-09-26 the exception went up and the whole page answered 500.
    def testFailingBlockStaysInItsBox(): Unit = {
      val failing = new logics.wikis.interpreters.TraitInterpreter {
        override val name: String = "Failing"
        override def toHtmlString(content: String)(implicit wikiContext: ContextWikiPage): String =
          throw new IllegalStateException("<boom>")
        override def toSeqLink(content: String)(implicit wikiContext: ContextWikiPage): Seq[models.tables.CalculatedLink] = Seq()
      }
      val html = Interpreters.render(failing, "#!Failing\nbody")
      assertEquals(html.startsWith("<div class=\"error\">#!Failing failed - "), true)
      assertEquals(html.contains("IllegalStateException"), true)
      // What the exception says is text, not markup.
      assertEquals(html.contains("<boom>"), false)
      assertEquals(html.contains("&lt;boom&gt;"), true)
      // A block that renders is untouched by the net.
      assertEquals(Interpreters.render(InterpreterWiki, "plain") == InterpreterWiki.toHtmlString("plain"), true)
    }; testFailingBlockStaysInItsBox()

    def testInterpreterTable(): Unit = {
      assertEquals(Interpreters.toHtmlString("#!table tsv\na\tb"), <table class="InterpreterTable wikiTableSimple tablesorter"><tbody><tr><td><div><p>a</p></div></td><td><div><p>b</p></div></td></tr></tbody></table>.toString())
      assertEquals(Interpreters.toHtmlString("#!table\n#!tsv\na\tb"), <table class="InterpreterTable wikiTableSimple tablesorter"><tbody><tr><td><div><p>a</p></div></td><td><div><p>b</p></div></td></tr></tbody></table>.toString())
      assertEquals(Interpreters.toHtmlString("#!table tsv 1\na\tb"), <table class="InterpreterTable wikiTableSimple tablesorter"><thead><tr><th><div><p>a</p></div></th><th><div><p>b</p></div></th></tr></thead><tbody></tbody></table>.toString())
      assertEquals(Interpreters.toHtmlString("#!table tsv 0 1\na\tb"), <table class="InterpreterTable wikiTableSimple tablesorter"><tbody><tr><th><div><p>a</p></div></th><td><div><p>b</p></div></td></tr></tbody></table>.toString())

      assertEquals(Interpreters.toHtmlString("#!table tsv some classes\na\tb"), <table class="InterpreterTable wikiTableSimple tablesorter some classes"><tbody><tr><td><div><p>a</p></div></td><td><div><p>b</p></div></td></tr></tbody></table>.toString())
      assertEquals(Interpreters.toHtmlString("#!table tsv 1 some classes\na\tb"), <table class="InterpreterTable wikiTableSimple tablesorter some classes"><thead><tr><th><div><p>a</p></div></th><th><div><p>b</p></div></th></tr></thead><tbody></tbody></table>.toString())
      assertEquals(Interpreters.toHtmlString("#!table tsv 1 tablesorter\na\tb"), <table class="InterpreterTable wikiTableSimple tablesorter"><thead><tr><th><div><p>a</p></div></th><th><div><p>b</p></div></th></tr></thead><tbody></tbody></table>.toString())
      assertEquals(Interpreters.toHtmlString("#!table tsv 0 1 some classes\na\tb"), <table class="InterpreterTable wikiTableSimple tablesorter some classes"><tbody><tr><th><div><p>a</p></div></th><td><div><p>b</p></div></td></tr></tbody></table>.toString())
      assertEquals(Interpreters.toHtmlString("#!table tsv 1 some \"bad\" classes\na\tb"), <table class="InterpreterTable wikiTableSimple tablesorter some classes"><thead><tr><th><div><p>a</p></div></th><th><div><p>b</p></div></th></tr></thead><tbody></tbody></table>.toString())
      assertEquals(Interpreters.toHtmlString("#!table tsv 1 \"bad\"\na\tb"), <table class="InterpreterTable wikiTableSimple tablesorter"><thead><tr><th><div><p>a</p></div></th><th><div><p>b</p></div></th></tr></thead><tbody></tbody></table>.toString())
      assertEquals(Interpreters.toHtmlString("#!table csv\n\"a,b\",c"), <table class="InterpreterTable wikiTableSimple tablesorter"><tbody><tr><td><div><p>a,b</p></div></td><td><div><p>c</p></div></td></tr></tbody></table>.toString())

      // What a cell has to look like to be a number: a spreadsheet's spelling, thousands separators
      // and all, a unit on the end allowed. The comma is a thousands separator, never a decimal mark.
      import logics.wikis.interpreters.InterpreterTable.parseNumber
      assertEquals(parseNumber("65,438,723"), Some(65438723.0))
      assertEquals(parseNumber("-7%"), Some(-7.0))
      assertEquals(parseNumber("+3"), Some(3.0))
      assertEquals(parseNumber("₩1,200.50"), Some(1200.5))
      assertEquals(parseNumber(" 42 "), Some(42.0))
      assertEquals(parseNumber("(1,234)"), Some(-1234.0))
      assertEquals(parseNumber("1,200원"), Some(1200.0))
      assertEquals(parseNumber("35명"), Some(35.0))
      assertEquals(parseNumber("12.5kg"), Some(12.5))
      assertEquals(parseNumber("3 items"), Some(3.0))
      assertEquals(parseNumber("5%p"), Some(5.0))
      assertEquals(parseNumber("1,23"), None)
      assertEquals(parseNumber("12 000"), None)
      assertEquals(parseNumber("2025-01"), None)
      assertEquals(parseNumber("2024년 3월"), None)
      assertEquals(parseNumber("1e5"), None)
      assertEquals(parseNumber(".5"), None)
      assertEquals(parseNumber("(1"), None)
      assertEquals(parseNumber("-"), None)
      assertEquals(parseNumber(""), None)
      assertEquals(parseNumber(null), None)
      // The Unicode minus is not a sign: tablesorter's digit parser drops it and would sort −3 as 3.
      assertEquals(parseNumber("−3"), None)
      // Digits past what a Double holds read as infinity, which would make NaN bars; not a number.
      assertEquals(parseNumber("1" + "0" * 400), None)

      // A column whose body cells are all numbers is numeric: each cell says so, and its header
      // names tablesorter's digit parser, so the column sorts by value whatever its first cell
      // holds. Nothing else about the table changes -- same classes, same inner HTML.
      val headNameScore = """<thead><tr><th><div><p>Name</p></div></th><th class="sorter-digit"><div><p>Score</p></div></th></tr></thead>"""
      assertEquals(
        Interpreters.toHtmlString("#!table tsv 1\nName\tScore\nA\t10\nB\t2"),
        """<table class="InterpreterTable wikiTableSimple tablesorter">""" + headNameScore + """<tbody><tr><td><div><p>A</p></div></td><td class="numeric"><div><p>10</p></div></td></tr>""" + "\n" +
          """<tr><td><div><p>B</p></div></td><td class="numeric"><div><p>2</p></div></td></tr></tbody></table>"""
      )
      // One word among the numbers and the column is text; so is a Unicode minus.
      assertEquals(
        Interpreters.toHtmlString("#!table tsv 1\nName\tScore\nA\t10\nB\tten"),
        """<table class="InterpreterTable wikiTableSimple tablesorter"><thead><tr><th><div><p>Name</p></div></th><th><div><p>Score</p></div></th></tr></thead><tbody><tr><td><div><p>A</p></div></td><td><div><p>10</p></div></td></tr>""" + "\n" +
          """<tr><td><div><p>B</p></div></td><td><div><p>ten</p></div></td></tr></tbody></table>"""
      )
      assertEquals(
        Interpreters.toHtmlString("#!table tsv 1\nName\tScore\nA\t10\nB\t−3"),
        """<table class="InterpreterTable wikiTableSimple tablesorter"><thead><tr><th><div><p>Name</p></div></th><th><div><p>Score</p></div></th></tr></thead><tbody><tr><td><div><p>A</p></div></td><td><div><p>10</p></div></td></tr>""" + "\n" +
          """<tr><td><div><p>B</p></div></td><td><div><p>−3</p></div></td></tr></tbody></table>"""
      )
      // An empty cell, "-" and "n/a" are a spreadsheet's "no value": the column stays numeric and
      // they are right-aligned with it, but they get no bar -- and an empty data-text, so that
      // tablesorter counts them as empty and sorts them last. A column that opened with "n/a"
      // used to sort as text.
      assertEquals(
        Interpreters.toHtmlString("#!table tsv 1 bar=2\nName\tScore\nA\t10\nB\t-\nC\tn/a\nD\t\nE\t#N/A"),
        """<table class="InterpreterTable wikiTableSimple tablesorter">""" + headNameScore + """<tbody><tr><td><div><p>A</p></div></td><td class="numeric dataBar" style="--bar-start:0.0%;--bar-end:100.0%"><div><p>10</p></div></td></tr>""" + "\n" +
          """<tr><td><div><p>B</p></div></td><td class="numeric" data-text=""><div><p>-</p></div></td></tr>""" + "\n" +
          """<tr><td><div><p>C</p></div></td><td class="numeric" data-text=""><div><p>n/a</p></div></td></tr>""" + "\n" +
          """<tr><td><div><p>D</p></div></td><td class="numeric" data-text=""><div></div></td></tr>""" + "\n" +
          """<tr><td><div><p>E</p></div></td><td class="numeric" data-text=""><div><p>#N/A</p></div></td></tr></tbody></table>"""
      )
      // A header column is a header whatever it holds; the body cell beside it is still judged.
      assertEquals(
        Interpreters.toHtmlString("#!table tsv 0 1\n1\t2"),
        """<table class="InterpreterTable wikiTableSimple tablesorter"><tbody><tr><th><div><p>1</p></div></th><td class="numeric"><div><p>2</p></div></td></tr></tbody></table>"""
      )
      // Every header-row cell of a numeric column names the parser -- tablesorter reads the
      // bottom header row -- and a header column's cells, in body rows, stay plain.
      assertEquals(
        Interpreters.toHtmlString("#!table tsv 2 1\nYear\tScore\n-\tpoints\n2024\t10\n2025\t2"),
        """<table class="InterpreterTable wikiTableSimple tablesorter"><thead><tr><th><div><p>Year</p></div></th><th class="sorter-digit"><div><p>Score</p></div></th></tr>""" + "\n" +
          """<tr><th><div><p>-</p></div></th><th class="sorter-digit"><div><p>points</p></div></th></tr></thead><tbody><tr><th><div><p>2024</p></div></th><td class="numeric"><div><p>10</p></div></td></tr>""" + "\n" +
          """<tr><th><div><p>2025</p></div></th><td class="numeric"><div><p>2</p></div></td></tr></tbody></table>"""
      )

      // bar= draws Excel's default data bar: from zero, in proportion, with the zero axis shared
      // by the column when it has a negative value -- -5 against 10 puts the axis at a third.
      val barred = """<table class="InterpreterTable wikiTableSimple tablesorter">""" + headNameScore + """<tbody><tr><td><div><p>A</p></div></td><td class="numeric dataBar" style="--bar-start:33.3%;--bar-end:100.0%"><div><p>10</p></div></td></tr>""" + "\n" +
        """<tr><td><div><p>B</p></div></td><td class="numeric dataBar negative" style="--bar-start:0.0%;--bar-end:33.3%"><div><p>-5</p></div></td></tr></tbody></table>"""
      assertEquals(Interpreters.toHtmlString("#!table tsv 1 bar=2\nName\tScore\nA\t10\nB\t-5"), barred)
      // By header text too, and the key case aside -- and bar= is an option, not a class, while
      // the words around it still are.
      assertEquals(Interpreters.toHtmlString("#!table tsv 1 bar=score\nName\tScore\nA\t10\nB\t-5"), barred)
      assertEquals(Interpreters.toHtmlString("#!table tsv 1 Bar=2\nName\tScore\nA\t10\nB\t-5"), barred)
      assertEquals(Interpreters.toHtmlString("#!table tsv 1 compact bar=2\nName\tScore\nA\t10\nB\t-5"), barred.replace("tablesorter\"", "tablesorter compact\""))
      // A header that is a number names its column once the number names no position.
      assertEquals(
        Interpreters.toHtmlString("#!table tsv 1 bar=2024\nItem\t2024\nA\t10\nB\t-5"),
        barred.replace("<p>Name</p>", "<p>Item</p>").replace("<p>Score</p>", "<p>2024</p>")
      )
      // Only the first header row names a column; the second row's text names nothing.
      assertEquals(
        Interpreters.toHtmlString("#!table tsv 2 bar=points\nName\tScore\n-\tpoints\nA\t10"),
        """<table class="InterpreterTable wikiTableSimple tablesorter">""" + headNameScore.replace("</tr></thead>", "</tr>\n<tr><th><div><p>-</p></div></th><th class=\"sorter-digit\"><div><p>points</p></div></th></tr></thead>") +
          """<tbody><tr><td><div><p>A</p></div></td><td class="numeric"><div><p>10</p></div></td></tr></tbody></table>"""
      )
      // Naming a text column, a column that is not there, or a number no Int holds draws nothing
      // and breaks nothing.
      assertEquals(
        Interpreters.toHtmlString("#!table tsv 1 bar=1,9,99999999999\nName\tScore\nA\t10"),
        """<table class="InterpreterTable wikiTableSimple tablesorter">""" + headNameScore + """<tbody><tr><td><div><p>A</p></div></td><td class="numeric"><div><p>10</p></div></td></tr></tbody></table>"""
      )
      // A unit on the number is the habit of Korean tables; the bar reads the number under it.
      assertEquals(
        Interpreters.toHtmlString("#!table tsv 1 bar=2\n품목\t가격\n가\t1,200원\n나\t300원"),
        """<table class="InterpreterTable wikiTableSimple tablesorter"><thead><tr><th><div><p>품목</p></div></th><th class="sorter-digit"><div><p>가격</p></div></th></tr></thead><tbody><tr><td><div><p>가</p></div></td><td class="numeric dataBar" style="--bar-start:0.0%;--bar-end:100.0%"><div><p>1,200원</p></div></td></tr>""" + "\n" +
          """<tr><td><div><p>나</p></div></td><td class="numeric dataBar" style="--bar-start:0.0%;--bar-end:25.0%"><div><p>300원</p></div></td></tr></tbody></table>"""
      )
      // bar=all: every numeric column, each on its own scale. Thousands separators and percent
      // signs are read as the sheet meant them. A repeated value still runs from zero, so it fills
      // its bar; a column of zeros has no range and gets none.
      assertEquals(
        Interpreters.toHtmlString("#!table tsv 1 bar=all\nMonth\tRevenue\tChange\tFlat\tZero\nJan\t65,438,723\t8%\t5\t0\nFeb\t119,526,353\t-8%\t5\t0"),
        """<table class="InterpreterTable wikiTableSimple tablesorter"><thead><tr><th><div><p>Month</p></div></th><th class="sorter-digit"><div><p>Revenue</p></div></th><th class="sorter-digit"><div><p>Change</p></div></th><th class="sorter-digit"><div><p>Flat</p></div></th><th class="sorter-digit"><div><p>Zero</p></div></th></tr></thead><tbody>""" +
          """<tr><td><div><p>Jan</p></div></td><td class="numeric dataBar" style="--bar-start:0.0%;--bar-end:54.7%"><div><p>65,438,723</p></div></td><td class="numeric dataBar" style="--bar-start:50.0%;--bar-end:100.0%"><div><p>8%</p></div></td><td class="numeric dataBar" style="--bar-start:0.0%;--bar-end:100.0%"><div><p>5</p></div></td><td class="numeric"><div><p>0</p></div></td></tr>""" + "\n" +
          """<tr><td><div><p>Feb</p></div></td><td class="numeric dataBar" style="--bar-start:0.0%;--bar-end:100.0%"><div><p>119,526,353</p></div></td><td class="numeric dataBar negative" style="--bar-start:0.0%;--bar-end:50.0%"><div><p>-8%</p></div></td><td class="numeric dataBar" style="--bar-start:0.0%;--bar-end:100.0%"><div><p>5</p></div></td><td class="numeric"><div><p>0</p></div></td></tr></tbody></table>"""
      )
    }; testInterpreterTable()

    def testInterpreterWiki(): Unit = {
      import models.tables.CalculatedLink

      assertEquals(InterpreterWiki.name, "Wiki")

      assertEquals(InterpreterWiki.inlineToHtmlString("""http://a.com"""), """<a href="http://a.com" target="_blank" rel="noopener">http://a.com</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""http://a.com$"""), """<a href="http://a.com$" target="_blank" rel="noopener">http://a.com$</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""http://a.com/some$thing"""), """<a href="http://a.com/some$thing" target="_blank" rel="noopener">http://a.com/some$thing</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""[http://a.com]"""), """<a href="http://a.com" target="_blank" rel="noopener">http://a.com</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""[http://a.com a com]"""), """<a href="http://a.com" target="_blank" rel="noopener">a com</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""[FrontPage]"""), """<a href="/w/FrontPage">FrontPage</a>""")
      // Since 2026-09-14 the space is part of the name, and a label comes after a pipe.
      assertEquals(InterpreterWiki.inlineToHtmlString("""[FrontPage Alias]"""), """<a href="/w/FrontPage Alias" class="missing" rel="nofollow">FrontPage Alias</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""[FrontPage|Alias]"""), """<a href="/w/FrontPage">Alias</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""[wiki:FrontPage]"""), """<a href="/w/FrontPage">FrontPage</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""[wiki:FrontPage Alias]"""), """<a href="/w/FrontPage">Alias</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""["Some Page"]"""), """<a href="/w/Some Page" class="missing" rel="nofollow">Some Page</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""["Some Page" Alias]"""), """<a href="/w/Some Page" class="missing" rel="nofollow">Alias</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""[schema:Schema]"""), """<a href="/w/schema:Schema" class="schema schema-link schema-schema">Schema</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""[schema:Movie]"""), """<a href="/w/schema:Movie" class="schema schema-link schema-movie">Movie</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""[schema:Schema Alias]"""), """<a href="/w/schema:Schema" class="schema schema-link schema-schema">Alias</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""["schema:Schema"]"""), """<a href="/w/schema:Schema" class="schema schema-link schema-schema">Schema</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""["schema:Schema" Alias]"""), """<a href="/w/schema:Schema" class="schema schema-link schema-schema">Alias</a>""")

      assertEquals(InterpreterWiki.inlineToHtmlString("""http://a.com/$   [http://a.com]  [http://a.com a com]"""), """<a href="http://a.com/$" target="_blank" rel="noopener">http://a.com/$</a>   <a href="http://a.com" target="_blank" rel="noopener">http://a.com</a>  <a href="http://a.com" target="_blank" rel="noopener">a com</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""http://a.com/some$thing   [http://a.com]  [http://a.com a com]"""), """<a href="http://a.com/some$thing" target="_blank" rel="noopener">http://a.com/some$thing</a>   <a href="http://a.com" target="_blank" rel="noopener">http://a.com</a>  <a href="http://a.com" target="_blank" rel="noopener">a com</a>""")

      assertEquals(InterpreterWiki.inlineToHtmlString("""\http://a.com"""), "http://a.com")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\http://a.com$"""), "http://a.com$")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\http://a.com/some$thing"""), "http://a.com/some$thing")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\[http://a.com]"""), "[http://a.com]")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\[http://a.com a com]"""), "[http://a.com a com]")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\[FrontPage]"""), "[FrontPage]")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\[FrontPage Alias]"""), "[FrontPage Alias]")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\[wiki:FrontPage]"""), "[wiki:FrontPage]")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\[wiki:FrontPage Alias]"""), "[wiki:FrontPage Alias]")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\[wiki:FrontPage]"""), "[wiki:FrontPage]")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\[wiki:FrontPage Alias]"""), "[wiki:FrontPage Alias]")

      assertEquals(InterpreterWiki.inlineToHtmlString("""\\http://a.com"""), """\\<a href="http://a.com" target="_blank" rel="noopener">http://a.com</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\\http://a.com$"""), """\\<a href="http://a.com$" target="_blank" rel="noopener">http://a.com$</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\\http://a.com/some$thing"""), """\\<a href="http://a.com/some$thing" target="_blank" rel="noopener">http://a.com/some$thing</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\\[http://a.com]"""), """\\<a href="http://a.com" target="_blank" rel="noopener">http://a.com</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\\[http://a.com a com]"""), """\\<a href="http://a.com" target="_blank" rel="noopener">a com</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\\[FrontPage]"""), """\\<a href="/w/FrontPage">FrontPage</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\\[FrontPage Alias]"""), """\\<a href="/w/FrontPage Alias" class="missing" rel="nofollow">FrontPage Alias</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\\[wiki:FrontPage]"""), """\\<a href="/w/FrontPage">FrontPage</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\\[wiki:FrontPage Alias]"""), """\\<a href="/w/FrontPage">Alias</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\\["Some Page"]"""), """\\<a href="/w/Some Page" class="missing" rel="nofollow">Some Page</a>""")
      assertEquals(InterpreterWiki.inlineToHtmlString("""\\["Some Page" Alias]"""), """\\<a href="/w/Some Page" class="missing" rel="nofollow">Alias</a>""")


      assertEquals(AhaMarkLink("""http://example.com""").toHtmlString(), """<a href="http://example.com" target="_blank" rel="noopener">http://example.com</a>""")
      assertEquals(AhaMarkLink("""AhaWiki""").toHtmlString(), """<a href="/w/AhaWiki">AhaWiki</a>""")
      assertEquals(AhaMarkLink("""#AhaWiki""").toHtmlString(), """<a href="#AhaWiki">#AhaWiki</a>""")
      assertEquals(AhaMarkLink("""?q=1""").toHtmlString(), """<a href="?q=1">?q=1</a>""")
      assertEquals(AhaMarkLink("""With:Colon""").toHtmlString(), """<a href="/w/With:Colon">With:Colon</a>""")
      assertEquals(AhaMarkLink("""With: Colon""").toHtmlString(), """<a href="/w/With: Colon">With: Colon</a>""")


      assertEquals(InterpreterWiki.extractLinkMarkup("""http://a.com""").toList, Seq(AhaMarkLink("""http://a.com""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""http://a.com$""").toList, Seq(AhaMarkLink("""http://a.com$""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""http://a.com/some$thing""").toList, Seq(AhaMarkLink("""http://a.com/some$thing""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""[http://a.com]""").toList, Seq(AhaMarkLink("""http://a.com""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""[http://a.com a com]""").toList, Seq(AhaMarkLink("""http://a.com""", """a com""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""[FrontPage]""").toList, Seq(AhaMarkLink("""FrontPage""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""[FrontPage Alias]""").toList, Seq(AhaMarkLink("""FrontPage Alias""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""[FrontPage|Alias]""").toList, Seq(AhaMarkLink("""FrontPage""", """Alias""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""[wiki:FrontPage]""").toList, Seq(AhaMarkLink("""wiki:FrontPage""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""[wiki:FrontPage Alias]""").toList, Seq(AhaMarkLink("""wiki:FrontPage""", """Alias""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""["Some Page"]""").toList, Seq(AhaMarkLink("""Some Page""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""["Some Page" Alias]""").toList, Seq(AhaMarkLink("""Some Page""", "Alias")))

      assertEquals(
        InterpreterWiki.extractLinkMarkup("""http://a.com/$   [http://a.com]  [http://a.com a com]""").toList,
        Seq(AhaMarkLink("""http://a.com/$"""), AhaMarkLink("""http://a.com"""), AhaMarkLink("""http://a.com""", """a com"""))
      )

      assertEquals(InterpreterWiki.extractLinkMarkup("""\http://a.com""").toList, Seq())
      assertEquals(InterpreterWiki.extractLinkMarkup("""\http://a.com$""").toList, Seq())
      assertEquals(InterpreterWiki.extractLinkMarkup("""\http://a.com/some$thing""").toList, Seq())
      assertEquals(InterpreterWiki.extractLinkMarkup("""\[http://a.com]""").toList, Seq())
      assertEquals(InterpreterWiki.extractLinkMarkup("""\[http://a.com a com]""").toList, Seq())
      assertEquals(InterpreterWiki.extractLinkMarkup("""\[FrontPage]""").toList, Seq())
      assertEquals(InterpreterWiki.extractLinkMarkup("""\[FrontPage Alias]""").toList, Seq())
      assertEquals(InterpreterWiki.extractLinkMarkup("""\[wiki:FrontPage]""").toList, Seq())
      assertEquals(InterpreterWiki.extractLinkMarkup("""\[wiki:FrontPage Alias]""").toList, Seq())
      assertEquals(InterpreterWiki.extractLinkMarkup("""\["SomePage"]""").toList, Seq())
      assertEquals(InterpreterWiki.extractLinkMarkup("""\["SomePage" Alias]""").toList, Seq())

      assertEquals(InterpreterWiki.extractLinkMarkup("""\\http://a.com""").toList, Seq(AhaMarkLink("""http://a.com""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""\\http://a.com$""").toList, Seq(AhaMarkLink("""http://a.com$""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""\\http://a.com/some$thing""").toList, Seq(AhaMarkLink("""http://a.com/some$thing""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""\\[http://a.com]""").toList, Seq(AhaMarkLink("""http://a.com""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""\\[http://a.com a com]""").toList, Seq(AhaMarkLink("""http://a.com""", """a com""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""\\[FrontPage]""").toList, Seq(AhaMarkLink("""FrontPage""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""\\[FrontPage Alias]""").toList, Seq(AhaMarkLink("""FrontPage Alias""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""\\[wiki:FrontPage]""").toList, Seq(AhaMarkLink("""wiki:FrontPage""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""\\[wiki:FrontPage Alias]""").toList, Seq(AhaMarkLink("""wiki:FrontPage""", """Alias""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""\\["SomePage"]""").toList, Seq(AhaMarkLink("""SomePage""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""\\["SomePage" Alias]""").toList, Seq(AhaMarkLink("""SomePage""", """Alias""")))

      assertEquals(InterpreterWiki.toSeqLink("[link]").toList, Seq(CalculatedLink("UnitTest", "link", "")))
      assertEquals(InterpreterWiki.toSeqLink("[link alias][b]").toList, Seq(CalculatedLink("UnitTest", "link alias", ""), CalculatedLink("UnitTest", "b", "")))
      assertEquals(InterpreterWiki.toSeqLink("[link|alias][b]").toList, Seq(CalculatedLink("UnitTest", "link", "alias"), CalculatedLink("UnitTest", "b", "")))

      // An anchor and a query point back at the page they are on, so neither is a link to a page.
      // Until 2026-10-03 the query was stored as one, to a page named "?q=10", and the
      // adjacent-pages graph asked /api/links for it and got a 404.
      assertEquals(InterpreterWiki.toSeqLink("[#section] [#section label] [?q=10] [?q=10 label] [b]").toList, Seq(CalculatedLink("UnitTest", "b", "")))

      // A link does not span lines, as the renderer draws it. An unclosed [ -- a half-open interval
      // in a note -- used to run to the next ] and become a link named after the text in between.
      assertEquals(InterpreterWiki.toSeqLink("index in [0, size) - half open\n\n== Counting ==\nsee [b]").toList, Seq(CalculatedLink("UnitTest", "b", "")))

      // WikiSyntaxPreview draws its Preview side with the interpreter it names, so its links are
      // that interpreter's. A JavaScript example shown through Vim has none -- until 2026-10-03 the
      // body was read as wiki text whatever the name, and InterpreterVim's [...Array(1000).keys()]
      // was stored as a link to a page of that name.
      assertEquals(logics.wikis.interpreters.InterpreterWikiSyntaxPreview.toSeqLink("#!WikiSyntaxPreview Vim javascript\n[...Array(1000).keys()].slice(1)\n").toList, Seq())
      assertEquals(logics.wikis.interpreters.InterpreterWikiSyntaxPreview.toSeqLink("#!WikiSyntaxPreview\n[b]\n").toList, Seq(CalculatedLink("UnitTest", "b", "")))
      assertEquals(logics.wikis.interpreters.InterpreterWikiSyntaxPreview.toSeqLink("#!WikiSyntaxPreview Wiki\n[b]\n").toList, Seq(CalculatedLink("UnitTest", "b", "")))

      // A block with no #! line is drawn as text, so it has no links either, and its wrapper says
      // Text. Until 2026-10-03 its links were read as wiki markup -- the [options...] of a usage
      // line was stored as a link to a page of that name -- and the wrapper said Wiki. A block that
      // names Wiki still links.
      assertEquals(InterpreterWiki.toSeqLink("[[[\ncurl [options...] <url>\n]]]\nsee [b]").toList, Seq(CalculatedLink("UnitTest", "b", "")))
      assertEquals(InterpreterWiki.toSeqLink("[[[#!Wiki\n[b]\n]]]").toList, Seq(CalculatedLink("UnitTest", "b", "")))
      assertEquals(InterpreterWiki.toHtmlString("[[[\ncurl [options...] <url>\n]]]").contains("""class="InterpreterRenderMetaWrapper Text""""), true)

    }; testInterpreterWiki()

    def testInterpreterSchema(): Unit = {
      import models.tables.CalculatedSchemaOrg

      assertEquals(InterpreterSchema.name, "Schema")

      {
        import models.tables.CalculatedSchemaOrg
        val schemaMarkup =
          """#!Schema Person
            |name	KIM, Aha
            |url	https://aha00a.com
            |memberOf	AhariseNotExists""".stripMargin

        val wikiMarkup =
          s"""[[[$schemaMarkup
             |]]]""".stripMargin

        val currentPageUrl = s"https://${request.host}/w/UnitTest"
        val interpreted =
          s"""<div class="schema InterpreterSchema"><dl vocab="https://schema.org/" typeof="Person">
             |        <h5 class="schemaClassTitle">
             |          <a href="/w/schema:Person" class="schema schema-link schema-person">Person</a>
             |        </h5>
             |        <div class="schemaFields">
             |          <div class="schemaFieldRow">
             |                <dt class="schemaFieldKey">
             |                  <a href="/w/schema:name" title="The name of the item." class="">Name</a>
             |                </dt>
             |                <dd class="schemaFieldValue" property="name"><a href="/w/KIM, Aha" class="missing" rel="nofollow">KIM, Aha</a></dd>
             |              </div><div class="schemaFieldRow">
             |                <dt class="schemaFieldKey">
             |                  <a href="/w/schema:url" title="URL of the item." class="">Url</a>
             |                </dt>
             |                <dd class="schemaFieldValue" property="url"><a href="https://aha00a.com" target="_blank" rel="noopener">https://aha00a.com</a></dd>
             |              </div><div class="schemaFieldRow">
             |                <dt class="schemaFieldKey">
             |                  <a href="/w/schema:memberOf" title="An Organization (or ProgramMembership) to which this Person or Organization belongs." class="">Member Of</a>
             |                </dt>
             |                <dd class="schemaFieldValue" property="memberOf"><a href="/w/AhariseNotExists" class="missing" rel="nofollow">AhariseNotExists</a></dd>
             |              </div>
             |        </div>
             |      </dl><script type="application/ld+json">{"@context":"https://schema.org","@type":"Person","mainEntityOfPage":"$currentPageUrl","@id":"$currentPageUrl","inLanguage":"en-US","name":"KIM, Aha","memberOf":"AhariseNotExists","url":"https://aha00a.com"}</script></div>""".stripMargin
        val interpretedWithWiki =
          s"""<div><div class="InterpreterRenderMetaWrapper Schema" style="position: relative;"
             |  data-edit-link="/w/UnitTest?action=edit&revision=0&lineStart=1&lineEnd=6"
             |  data-line-start="1"
             |  data-line-end="6"
             |  data-edit-title="Edit (r0, L1-L5)">
             |  <div class="InterpreterRenderContent">$interpreted</div>
             |</div></div>""".stripMargin

        assertEquals(InterpreterSchema.toHtmlString(schemaMarkup), interpreted)
        assertEquals(Interpreters.toHtmlString(wikiMarkup), interpretedWithWiki)

        val extractWordResult = Seq("Person", "Name", "KIM,", "Aha", "Url", "https://aha00a.com", "Member", "Of", "AhariseNotExists")
        assertEquals(Interpreters.toText(wikiMarkup).split(" ").toSeq, extractWordResult)

        assertEquals(InterpreterSchema.toSeqLink(schemaMarkup), Seq())
        assertEquals(Interpreters.toSeqLink(wikiMarkup), Seq())

        val extractSchemaResult = Seq(
          CalculatedSchemaOrg("UnitTest", "Person", "", "schema:Person"),
          CalculatedSchemaOrg("UnitTest", "Person", "name", "KIM, Aha"),
          CalculatedSchemaOrg("UnitTest", "Person", "memberOf", "AhariseNotExists")
        )
        assertEquals(InterpreterSchema.toSeqSchemaOrg(schemaMarkup), extractSchemaResult)
        assertEquals(Interpreters.toSeqSchemaOrg(wikiMarkup), extractSchemaResult)
      }


      assertEquals(
        InterpreterSchema.toSeqSchemaOrg(
          """#!Schema WebApplication
            |name	AhaWiki
            |url	https://wiki.aha00a.com/w/AhaWiki
            |featureList	https://wiki.aha00a.com/w/AhaWikiFeature
            |applicationCategory	Wiki
            |datePublished	2015-10-21
            |""".stripMargin
        ).toList,
        Seq(
          CalculatedSchemaOrg("UnitTest", "WebApplication", "", "schema:WebApplication"),
          CalculatedSchemaOrg("UnitTest", "WebApplication", "name", "AhaWiki"),
          CalculatedSchemaOrg("UnitTest", "WebApplication", "applicationCategory", "Wiki"),
          CalculatedSchemaOrg("UnitTest", "WebApplication", "datePublished", "2015-10-21"),
          CalculatedSchemaOrg("UnitTest", "WebApplication", "datePublished", "2015-10"),
        )
      )
    }; testInterpreterSchema()

  }
}
