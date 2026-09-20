package com.aha00a.tests.unit

import com.aha00a.tests.TestUtil
import logics.CalculatedSchemaOrg
import logics.CalculatedSchemaOrg

object SchemaOrgUnit {
  def run(testUtil: TestUtil): Unit = {
    import testUtil.assertEquals

    // After CalculatedSchemaOrg.isSchemaOrgTerm, which drops the namespaced terms the release
    // bundles from other vocabularies and the two the prefix stripping renames to `type` and
    // `label`, and plus the classes in public/schema.org/custom.jsonld.
    // test/schema-org-vocabulary.test.mjs asserts the schema.org side of these from the file.
    assertEquals(logics.CalculatedSchemaOrg.mapAll.size, 3027)
    assertEquals(logics.CalculatedSchemaOrg.mapClass.size, 943)
    assertEquals(logics.CalculatedSchemaOrg.mapProperty.size, 1538)

    // A custom class is a class like any other once merged: it has the parent it declares, so it
    // inherits CreativeWork's properties and sits under it in the tree rather than in the flat
    // "Custom" list renderExistingPages falls back to.
    assertEquals(logics.CalculatedSchemaOrg.seqCustom.map(_.id), Seq("Standard", "Poem", "Whiskey", "Cognac"))
    assertEquals(logics.CalculatedSchemaOrg.getClassHierarchy("Standard"), Seq("Standard", "CreativeWork", "Thing"))
    assertEquals(logics.CalculatedSchemaOrg.getClassHierarchy("Poem"), Seq("Poem", "CreativeWork", "Thing"))
    assertEquals(logics.CalculatedSchemaOrg.getClassHierarchy("Whiskey"), Seq("Whiskey", "Product", "Thing"))
    assertEquals(logics.CalculatedSchemaOrg.getClassHierarchy("Cognac"), Seq("Cognac", "Product", "Thing"))

    // Every custom class must name a parent schema.org really has, or it is grafted onto nothing
    // and its pages fall back to the flat list without saying why.
    logics.CalculatedSchemaOrg.seqCustom.foreach { custom =>
      assertEquals(custom.subClassOf.size, 1)
      assertEquals(logics.CalculatedSchemaOrg.mapClass.isDefinedAt(custom.subClassOf.head), true)
    }

    // The graft into the tree, which is a separate file from the vocabulary: without it Standard
    // would have a parent in the maps and still be listed in the flat "Custom" section here.
    {
      val rendered = CalculatedSchemaOrg.renderExistingPages(Map("Standard" -> Seq("ISO 8601")))
      assertEquals(rendered.contains("= Custom"), false)
      assertEquals(rendered.contains("""["schema:CreativeWork" Creative Work]"""), true)
      assertEquals(rendered.contains("""["schema:Standard" Standard] (1)"""), true)
    }

    // A class in neither the vocabulary nor custom.jsonld still renders, in that flat section.
    // The example used to be Whiskey, until Whiskey was given a parent and left it.
    assertEquals(
      CalculatedSchemaOrg.renderExistingPages(Map("NotAClassAnywhere" -> Seq("Somewhere"))).contains("= Custom"),
      true
    )

    // Ours and schema.org's, told apart — the class pages link to schema.org only for the latter,
    // because https://schema.org/Standard is a 404.
    assertEquals(CalculatedSchemaOrg.isCustom("Standard"), true)
    assertEquals(CalculatedSchemaOrg.isCustom("Poem"), true)
    assertEquals(CalculatedSchemaOrg.isCustom("CreativeWork"), false)
    assertEquals(CalculatedSchemaOrg.isCustom("ShortStory"), false)
    assertEquals(CalculatedSchemaOrg.isCustom("NotAClassAnywhere"), false)

    // And the three that just gained one are out of it, under the parents they declare.
    {
      val rendered = CalculatedSchemaOrg.renderExistingPages(Map(
        "Poem" -> Seq("꽃"),
        "Whiskey" -> Seq("The Macallan Sherry Oak 12 Years Old"),
        "Cognac" -> Seq("Rémy Martin"),
      ))
      assertEquals(rendered.contains("= Custom"), false)
      assertEquals(rendered.contains("""["schema:Product" Product]"""), true)
      assertEquals(rendered.contains("""["schema:Poem" Poem] (1)"""), true)
    }


    val schemaType: logics.CalculatedSchemaOrg.SchemaType = logics.CalculatedSchemaOrg.mapAll("Movie")
    assertEquals(schemaType.toXmlSpan().toString(), """<a href="/w/schema:Movie" title="A movie." class="">Movie</a>""")

    assertEquals(logics.CalculatedSchemaOrg.renderExistingPages(Map()), "")

    assertEquals(
      CalculatedSchemaOrg.renderExistingPages(Map(
        "Thing" -> Seq("Something")
      )),
      """= ["schema:Thing" Thing] (1) = #Thing
        |<Columns count="3" gap="16" minWidth="220">
        | 1. ["Something"]
        |</Columns>
        |
        |""".stripMargin
    )

    assertEquals(
      CalculatedSchemaOrg.renderExistingPages(Map(
        "Thing" -> Seq("Something"),
        "Movie" -> Seq("AwesomeMovie1", "AwesomeMovie2"),
        "Person" -> Seq("Someone1", "Someone2"),
        "WebSite" -> Seq("Site1", "Site2")
      )),
      """= ["schema:Thing" Thing] (7) = #Thing
        |<Columns count="3" gap="16" minWidth="220">
        | 1. ["Something"]
        |</Columns>
        |== ["schema:CreativeWork" Creative Work] (4) == #CreativeWork
        |
        |=== ["schema:Movie" Movie] (2) === #Movie
        |<Columns count="3" gap="16" minWidth="220">
        | 1. ["AwesomeMovie1"]
        | 1. ["AwesomeMovie2"]
        |</Columns>
        |
        |
        |=== ["schema:WebSite" Web Site] (2) === #WebSite
        |<Columns count="3" gap="16" minWidth="220">
        | 1. ["Site1"]
        | 1. ["Site2"]
        |</Columns>
        |
        |
        |
        |== ["schema:Person" Person] (2) == #Person
        |<Columns count="3" gap="16" minWidth="220">
        | 1. ["Someone1"]
        | 1. ["Someone2"]
        |</Columns>
        |
        |
        |""".stripMargin
    )

    assertEquals(
      CalculatedSchemaOrg.renderExistingPages(Map(
        "Thing" -> Seq("Something"),
        "CustomClass" -> Seq("Anything")
      )),
      """
        |= ["schema:Thing" Thing] (1) = #Thing
        |<Columns count="3" gap="16" minWidth="220">
        | 1. ["Something"]
        |</Columns>
        |
        |
        |
        |= Custom
        |== ["schema:CustomClass" Custom Class] (1) == #CustomClass
        |<Columns count="3" gap="16" minWidth="220">
        | 1. ["Anything"]
        |</Columns>
        |
        |""".stripMargin
    )
  }
}
