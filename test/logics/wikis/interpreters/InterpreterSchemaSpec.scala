package logics.wikis.interpreters

import models.ContextWikiPage
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.play.guice.GuiceOneAppPerTest
import provider.EmptyContextWikiPage
import provider.RealContextWikiPage

class InterpreterSchemaSpecExample extends AnyFreeSpec with GuiceOneAppPerTest with RealContextWikiPage {
  implicit val contextWikiPage: ContextWikiPage = createContextWikiPage();
  "name" in {
    assert(InterpreterSchema.name === "Schema")
  }
}

class InterpreterSchemaSpec extends AnyFreeSpec with EmptyContextWikiPage {
  "name" in {
    assert(InterpreterSchema.name === "Schema")
  }
}
