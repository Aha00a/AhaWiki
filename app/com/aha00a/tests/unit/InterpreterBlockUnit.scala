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
      assertEquals(InterpreterWiki.inlineToHtmlString("""[FrontPage Alias]"""), """<a href="/w/FrontPage">Alias</a>""")
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
      assertEquals(InterpreterWiki.inlineToHtmlString("""\\[FrontPage Alias]"""), """\\<a href="/w/FrontPage">Alias</a>""")
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
      assertEquals(InterpreterWiki.extractLinkMarkup("""[FrontPage Alias]""").toList, Seq(AhaMarkLink("""FrontPage""", """Alias""")))
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
      assertEquals(InterpreterWiki.extractLinkMarkup("""\\[FrontPage Alias]""").toList, Seq(AhaMarkLink("""FrontPage""", """Alias""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""\\[wiki:FrontPage]""").toList, Seq(AhaMarkLink("""wiki:FrontPage""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""\\[wiki:FrontPage Alias]""").toList, Seq(AhaMarkLink("""wiki:FrontPage""", """Alias""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""\\["SomePage"]""").toList, Seq(AhaMarkLink("""SomePage""")))
      assertEquals(InterpreterWiki.extractLinkMarkup("""\\["SomePage" Alias]""").toList, Seq(AhaMarkLink("""SomePage""", """Alias""")))

      assertEquals(InterpreterWiki.toSeqLink("[link]").toList, Seq(CalculatedLink("UnitTest", "link", "")))
      assertEquals(InterpreterWiki.toSeqLink("[link alias][b]").toList, Seq(CalculatedLink("UnitTest", "link", "alias"), CalculatedLink("UnitTest", "b", "")))

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
