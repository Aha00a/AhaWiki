package logics.wikis.interpreters

import logics.wikis.RenderingMode
import models.ContextWikiPage
import models.RequestWrapper
import org.scalatest.freespec.AnyFreeSpec
import models.tables.Site

//noinspection NameBooleanParameters
class InterpreterSchemaSpec extends AnyFreeSpec {
  implicit val site: Site = Site.notFound
  implicit val provider: RequestWrapper = RequestWrapper.empty
  implicit val contextWikiPage: ContextWikiPage = new ContextWikiPage(Seq("UnitTest"), RenderingMode.Normal)(null, null, null, null, null)

  "name" in {
    assert(InterpreterSchema.name === "Schema")
  }
}
