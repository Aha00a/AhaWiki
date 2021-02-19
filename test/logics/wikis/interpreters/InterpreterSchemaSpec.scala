package logics.wikis.interpreters

import logics.wikis.RenderingMode
import models.WikiContext
import models.WikiContext.Provider
import org.scalatest.freespec.AnyFreeSpec
import models.tables.Site

//noinspection NameBooleanParameters
class InterpreterSchemaSpec extends AnyFreeSpec {
  implicit val site: Site = Site(-1, "")
  implicit val provider: Provider = Provider.empty
  implicit val wikiContext: WikiContext = new WikiContext(Seq("UnitTest"), RenderingMode.Normal)(null, null, null, null, null)

  "name" in {
    assert(InterpreterSchema.name === "Schema")
  }
}
