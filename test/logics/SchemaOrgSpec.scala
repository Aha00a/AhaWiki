package logics

import org.scalatest.freespec.AnyFreeSpec

class SchemaOrgSpec extends AnyFreeSpec {

  "properties" in {
    assert(CalculatedSchemaOrg.mapAll.size == 2853)
    assert(CalculatedSchemaOrg.mapClass.size == 906)
    assert(CalculatedSchemaOrg.mapProperty.size == 1469)
  }
  "SchemaType" - {
    "toXmlSpan" in {
      val schemaType: CalculatedSchemaOrg.SchemaType = CalculatedSchemaOrg.mapAll("Movie")
      assert(schemaType.toXmlSpan().toString() === """<a href="/w/schema:Movie" title="A movie." class="">Movie</a>""")
    }
  }


  "renderExistingPages" in {

    assert(CalculatedSchemaOrg.renderExistingPages(Map()) === "")

    assert(CalculatedSchemaOrg.renderExistingPages(Map(
      "Thing" -> Seq("Something"),
    )) ===
      """= ["schema:Thing" Thing]
        | 1. ["Something"]
        |
        |""".stripMargin)

    assert(CalculatedSchemaOrg.renderExistingPages(Map(
      "Thing" -> Seq("Something"),
      "Movie" -> Seq("AwesomeMovie1", "AwesomeMovie2"),
      "Person" -> Seq("Someone1", "Someone2"),
      "WebSite" -> Seq("Site1", "Site2"),
    )) ===
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
        |""".stripMargin)

    assert(CalculatedSchemaOrg.renderExistingPages(Map(
      "Thing" -> Seq("Something"),
      "CustomClass" -> Seq("Anything"),
    )) ===
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
        |""".stripMargin)

  }
}
