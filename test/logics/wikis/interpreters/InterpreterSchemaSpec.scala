package logics.wikis.interpreters

import models.ContextWikiPage
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.play.guice.GuiceOneAppPerTest
import provider.EmptyContextWikiPage
import provider.RealContextWikiPage

class InterpreterSchemaSpecExample extends AnyFreeSpec with GuiceOneAppPerTest with RealContextWikiPage {
  implicit val contextWikiPage: ContextWikiPage = createContextWikiPage();
  "name" in {
    assert(InterpreterSchema.name == "Schema")
  }
}

class InterpreterSchemaSpec extends AnyFreeSpec with EmptyContextWikiPage {
  "name" in {
    assert(InterpreterSchema.name == "Schema")
  }

  "expandAddress" in {
    assert(InterpreterSchema.expandAddress("서울특별시 마포구 망원동 999-999") == Seq(
      Seq("서울특별시"),
      Seq("서울특별시", "마포구"),
      Seq("서울특별시", "마포구", "망원동"),
      Seq("서울특별시", "마포구", "망원동", "999-999"),
    ))
  }
}
