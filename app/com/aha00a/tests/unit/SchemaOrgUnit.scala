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
      """= ["schema:Thing" Thing]
        | 1. ["Something"]
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
      """= ["schema:Thing" Thing]
        | 1. ["Something"]
        |== ["schema:CreativeWork" Creative Work]
        |
        |=== ["schema:Movie" Movie]
        | 1. ["AwesomeMovie1"]
        | 1. ["AwesomeMovie2"]
        |
        |
        |=== ["schema:WebSite" Web Site]
        | 1. ["Site1"]
        | 1. ["Site2"]
        |
        |
        |
        |== ["schema:Person" Person]
        | 1. ["Someone1"]
        | 1. ["Someone2"]
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
        |= ["schema:Thing" Thing]
        | 1. ["Something"]
        |
        |
        |
        |= Custom
        |== ["schema:CustomClass" Custom Class]
        | 1. ["Anything"]
        |
        |""".stripMargin
    )
  }
}
