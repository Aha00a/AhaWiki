package logics.wikis.interpreters

import logics.AhaWikiInjects
import logics.wikis.RenderingMode
import models.WikiContext
import models.WikiContext.Provider
import org.scalatest.freespec.AnyFreeSpec
import models.tables.Site

//noinspection NameBooleanParameters
class InterpreterSchemaSpec extends AnyFreeSpec {
  implicit val site: Site = Site(-1, "")
  implicit val ahaWikiInjects: AhaWikiInjects = AhaWikiInjects()(null, null, null, null, null)
  implicit val provider: Provider = Provider.empty
  implicit val wikiContext: WikiContext = new WikiContext(Seq("UnitTest"), RenderingMode.Normal)

  "name" in {
    assert(InterpreterSchema.name === "Schema")
  }
}
