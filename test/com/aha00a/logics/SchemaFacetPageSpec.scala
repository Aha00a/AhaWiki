package com.aha00a.logics

import logics.DefaultPageLogic
import logics.DefaultPageLogic.SchemaFacet
import models.tables.CalculatedSchemaOrg
import org.scalatest.freespec.AnyFreeSpec

/**
 * `schema:TVSeries:publisher`, `schema:publisher:JTBC` and `schema:TVSeries:publisher:JTBC`: which
 * names exist, and what their pages say given the rows. Reading the rows needs a database and is not covered here.
 */
class SchemaFacetPageSpec extends AnyFreeSpec {
  private def row(page: String, value: String): CalculatedSchemaOrg =
    CalculatedSchemaOrg(page, "TVSeries", "publisher", value)

  private val rows = Seq(row("A", "KBS"), row("B", "JTBC"), row("C", "JTBC"))

  private def content(value: Option[String], of: Seq[CalculatedSchemaOrg] = rows): String =
    DefaultPageLogic.schemaFacetPageContent(SchemaFacet(Some("TVSeries"), "publisher", value), of)

  private def everyClass(value: String, of: Seq[CalculatedSchemaOrg]): String =
    DefaultPageLogic.schemaFacetPageContent(SchemaFacet(None, "publisher", Some(value)), of)

  "which names exist" - {
    "a class and a property, with or without a value" in {
      assert(DefaultPageLogic.parseSchemaFacet("schema:TVSeries:publisher") === Some(SchemaFacet(Some("TVSeries"), "publisher", None)))
      assert(DefaultPageLogic.parseSchemaFacet("schema:TVSeries:publisher:JTBC") === Some(SchemaFacet(Some("TVSeries"), "publisher", Some("JTBC"))))
      assert(DefaultPageLogic.isDefined("schema:TVSeries:publisher"))
      assert(DefaultPageLogic.isDefined("schema:TVSeries:publisher:JTBC"))
    }

    "a property and a value, in every class" in {
      assert(DefaultPageLogic.parseSchemaFacet("schema:publisher:JTBC") === Some(SchemaFacet(None, "publisher", Some("JTBC"))))
      assert(DefaultPageLogic.isDefined("schema:publisher:JTBC"))
    }

    "a value may itself hold a colon" in {
      assert(DefaultPageLogic.parseSchemaFacet("schema:TVSeries:url:https://example.com") === Some(SchemaFacet(Some("TVSeries"), "url", Some("https://example.com"))))
      assert(DefaultPageLogic.parseSchemaFacet("schema:url:https://example.com") === Some(SchemaFacet(None, "url", Some("https://example.com"))))
    }

    "an empty value is the page without one" in {
      assert(DefaultPageLogic.parseSchemaFacet("schema:TVSeries:publisher:") === Some(SchemaFacet(Some("TVSeries"), "publisher", None)))
    }

    "a term the vocabulary does not have is not a page" in {
      assert(!DefaultPageLogic.isDefined("schema:TVSeries:KR"))
      assert(!DefaultPageLogic.isDefined("schema:NoSuchClass:publisher:JTBC"))
    }

    "after a class the next term must be a property" in {
      assert(!DefaultPageLogic.isDefined("schema:TVSeries:Movie"))
      assert(!DefaultPageLogic.isDefined("schema:TVSeries:Movie:JTBC"))
    }

    "after a property the next term is a value, whatever it looks like" in {
      assert(DefaultPageLogic.parseSchemaFacet("schema:publisher:TVSeries") === Some(SchemaFacet(None, "publisher", Some("TVSeries"))))
    }

    "a value cannot span lines" in {
      assert(!DefaultPageLogic.isDefined("schema:TVSeries:publisher:a\n= b"))
    }

    "the single-term pages are not affected" in {
      assert(DefaultPageLogic.isDefined("schema:TVSeries"))
      assert(DefaultPageLogic.isDefined("schema:publisher"))
      assert(DefaultPageLogic.isDefined("schema:Schema"))
    }
  }

  "a value's page" - {
    "lists the pages holding that value and no others" in {
      val page = content(Some("JTBC"))

      assert(page.contains(""" 1. ["B"]"""))
      assert(page.contains(""" 1. ["C"]"""))
      assert(!page.contains(""" 1. ["A"]"""))
    }

    "lists a page once when two rows hold the same value" in {
      val page = content(Some("JTBC"), Seq(row("B", "JTBC"), row("B", "JTBC")))

      assert(page.split("""\["B"\]""", -1).length === 2)
    }

    "links back to every value" in {
      assert(content(Some("JTBC")).contains("""["schema:TVSeries:publisher" All values]"""))
    }

    "with no match lists the values in use instead of being empty" in {
      val page = content(Some("대한민국"))

      assert(page.contains("No page has this value."))
      assert(page.contains("""["schema:TVSeries:publisher:JTBC" JTBC]"""))
      assert(page.contains("""["schema:TVSeries:publisher:KBS" KBS]"""))
    }

    "with no rows at all says so" in {
      assert(content(Some("JTBC"), Seq()).contains("No page has this property."))
    }

    "never turns the value from the URL into markup" in {
      val page = content(Some("[[Html(<b>x</b>)]]"))

      assert(!page.contains("[[Html"))
      assert(page.linesIterator.next() === "= TV Series: Publisher: ((Html(<b>x</b>)))")
    }
  }

  "a value in every class" - {
    val mixed = Seq(row("A", "JTBC"), CalculatedSchemaOrg("M", "Movie", "publisher", "JTBC"), row("B", "KBS"))

    "groups the pages by class, each heading narrowing to that class" in {
      val page = everyClass("JTBC", mixed)

      assert(page.contains("""== ["schema:Movie:publisher:JTBC" Movie]"""))
      assert(page.contains("""== ["schema:TVSeries:publisher:JTBC" TV Series]"""))
      assert(!page.contains(""" 1. ["B"]"""))
    }

    "with no match offers the values under the same name" in {
      assert(everyClass("대한민국", mixed).contains("""["schema:publisher:KBS" KBS]"""))
    }

    "never turns the value from the URL into markup" in {
      assert(!everyClass("[[Html(<b>x</b>)]]", mixed).contains("[[Html"))
    }
  }

  "a class's properties" - {
    "link to the class's page of each, counting pages once" in {
      val section = DefaultPageLogic.schemaClassPropertiesSection("TVSeries", Seq(
        CalculatedSchemaOrg("A", "TVSeries", "actor", "x"),
        CalculatedSchemaOrg("A", "TVSeries", "actor", "y"),
        CalculatedSchemaOrg("B", "TVSeries", "actor", "x"),
      ))

      assert(section.contains("""["schema:TVSeries:actor" Actor] (2)"""))
    }

    "leave out a label the vocabulary does not have" in {
      assert(DefaultPageLogic.schemaClassPropertiesSection("Movie", Seq(CalculatedSchemaOrg("A", "Movie", "Cinematography", "x"))) === "")
    }
  }

  "the property's page" - {
    "groups the pages by value, sorted" in {
      val page = content(None)

      assert(page.indexOf("""== ["JTBC" JTBC]""") < page.indexOf("""== ["KBS" KBS]"""))
      assert(page.contains(""" 1. ["A"]"""))
      assert(page.contains(""" 1. ["B"]"""))
    }

    "with no rows says so" in {
      assert(content(None, Seq()).contains("No page has this property."))
    }
  }
}
