package logics

import org.scalatest.freespec.AnyFreeSpec

class SchemaOrgSpec extends AnyFreeSpec {
  "incrGet" in {
    assert(SchemaOrg.mapAll.size == 2381)
    assert(SchemaOrg.mapClass.size == 818)
    assert(SchemaOrg.mapProperty.size == 1278)
  }
}
