package logics

import org.scalatest.freespec.AnyFreeSpec

class SchemaOrgSpec extends AnyFreeSpec {

  "properties" in {
    assert(SchemaOrg.mapAll.size == 2381)
    assert(SchemaOrg.mapClass.size == 818)
    assert(SchemaOrg.mapProperty.size == 1278)
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
        | * ["Something"]
        |
        |""".stripMargin)

    assert(SchemaOrg.renderExistingPages(Map(
      "Thing" -> Seq("Something"),
      "Movie" -> Seq("AwesomeMovie1", "AwesomeMovie2"),
      "Person" -> Seq("Someone1", "Someone2"),
      "WebSite" -> Seq("Site1", "Site2"),
    )) ===
      """= ["schema:Thing" Thing]
        | * ["Something"]
        |== ["schema:CreativeWork" Creative Work]
        |
        |=== ["schema:Movie" Movie]
        | * ["AwesomeMovie1"]
        | * ["AwesomeMovie2"]
        |
        |
        |=== ["schema:WebSite" Web Site]
        | * ["Site1"]
        | * ["Site2"]
        |
        |
        |
        |== ["schema:Person" Person]
        | * ["Someone1"]
        | * ["Someone2"]
        |
        |
        |""".stripMargin)

    assert(SchemaOrg.renderExistingPages(Map(
      "Thing" -> Seq("Something"),
      "CustomClass" -> Seq("Anything"),
    )) ===
      """
        |= ["schema:Thing" Thing]
        | * ["Something"]
        |
        |
        |
        |= Custom
        |== ["schema:CustomClass" Custom Class]
        | * ["Anything"]
        |
        |""".stripMargin)

  }
}
