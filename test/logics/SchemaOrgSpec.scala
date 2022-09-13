package logics

import org.scalatest.freespec.AnyFreeSpec

class SchemaOrgSpec extends AnyFreeSpec {

  "properties" in {
    assert(SchemaOrg.mapAll.size == 2801)
    assert(SchemaOrg.mapClass.size == 893)
    assert(SchemaOrg.mapProperty.size == 1448)
  }
  "SchemaType" - {
    "toXmlSpan" in {
      val schemaType: SchemaOrg.SchemaType = SchemaOrg.mapAll("Movie")
      assert(schemaType.toXmlSpan().toString() === """<a href="/w/schema:Movie" title="A movie." class="">Movie </a>""")
    }
  }


  "renderExistingPages" in {

    assert(SchemaOrg.renderExistingPages(Map()) === "")

    assert(SchemaOrg.renderExistingPages(Map(
      "Thing" -> Seq("Something"),
    )) ===
      """= ["schema:Thing" Thing]
        | 1. ["Something"]
        |
        |""".stripMargin)

    assert(SchemaOrg.renderExistingPages(Map(
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

    assert(SchemaOrg.renderExistingPages(Map(
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
