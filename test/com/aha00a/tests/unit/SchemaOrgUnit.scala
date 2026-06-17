package com.aha00a.tests.unit

import com.aha00a.tests.TestUtil
import logics.CalculatedSchemaOrg
import logics.CalculatedSchemaOrg

object SchemaOrgUnit {
  def run(testUtil: TestUtil): Unit = {
    import testUtil.assertEquals

    assertEquals(logics.CalculatedSchemaOrg.mapAll.size, 2853)
    assertEquals(logics.CalculatedSchemaOrg.mapClass.size, 906)
    assertEquals(logics.CalculatedSchemaOrg.mapProperty.size, 1469)


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
