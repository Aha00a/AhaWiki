package logics.wikis.interpreters

import logics.wikis.RenderingMode
import models.ContextWikiPage
import models.RequestWrapper
import org.scalatest.freespec.AnyFreeSpec
import models.tables.Site
import provider.EmptyContextWikiPage

//noinspection NameBooleanParameters
class InterpreterSchemaSpec extends AnyFreeSpec with EmptyContextWikiPage {
  "name" in {
    assert(InterpreterSchema.name === "Schema")
  }
}
