package logics.wikis.interpreters

import logics.AhaWikiInjects
import logics.wikis.RenderingMode
import models.WikiContext
import models.WikiContext.Provider
import org.scalatest.freespec.AnyFreeSpec

//noinspection NameBooleanParameters
class InterpreterSchemaSpec extends AnyFreeSpec {
  implicit val ahaWikiInjects: AhaWikiInjects = AhaWikiInjects()(null, null, null, null)
  implicit val wikiContext: WikiContext = new WikiContext(Seq("UnitTest"), RenderingMode.Normal)(ahaWikiInjects, Provider.empty)

  "name" in {
    assert(InterpreterSchema.name === "Schema")
  }
}
