package logics

import org.scalatest.freespec.AnyFreeSpec

class SchemaOrgSpec extends AnyFreeSpec {
  "incrGet" in {
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
}
