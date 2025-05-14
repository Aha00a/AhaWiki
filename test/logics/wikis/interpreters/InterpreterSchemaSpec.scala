package logics.wikis.interpreters

import logics.wikis.interpreters.InterpreterSchema.ParseResult
import models.ContextWikiPage
import models.PageContent
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.play.guice.GuiceOneAppPerTest
import provider.EmptyContextWikiPage
import provider.RealContextWikiPage

//class InterpreterSchemaSpecExample extends AnyFreeSpec with GuiceOneAppPerTest with RealContextWikiPage {
//  implicit val contextWikiPage: ContextWikiPage = createContextWikiPage()
//  "name" in {
//    assert(InterpreterSchema.name == "Schema")
//  }
//}

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

  "mergeFields" in {
    val seqSeqField: Seq[Seq[String]] = Seq(
      Seq("name", "Aha00a"),
      Seq("startDate", "1982-10-23"),
      Seq("endDate", "2099-10-23"),
      Seq("location", "Seoul"),
      Seq("actor", "Actor1", "Actor2", "Actor3Alone"),
      Seq("character", "Character1", "Character2"),
      Seq("location", "do not merge"),
      Seq("actor", "Actor1", "Actor2", "Actor3Alone"),
      Seq("location", "do not merge"),
      Seq("character", "Character1", "Character2"),
      Seq("location", "do not merge"),
      Seq("startDate", "A", "B"),
      Seq("endDate", "C", "D", "E"),
      Seq("location", "do not merge"),
    )

    val mapPair: Map[String, String] = Seq(
      "birthDate" -> "deathDate",
      "startDate" -> "endDate",
      "actor" -> "character",
    ).toMap


    val merged = InterpreterSchema.mergeFields(seqSeqField, mapPair)

    merged shouldBe Seq(
      Seq("name", "Aha00a"),
      Seq("startDate endDate", "1982-10-23", "2099-10-23"),
      Seq("location", "Seoul"),
      Seq("actor character", "Actor1", "Character1", "Actor2", "Character2", "Actor3Alone", ""),
      Seq("location", "do not merge"),
      Seq("actor", "Actor1", "Actor2", "Actor3Alone"),
      Seq("location", "do not merge"),
      Seq("character", "Character1", "Character2"),
      Seq("location", "do not merge"),
      Seq("startDate endDate", "A", "C", "B", "D", "", "E"),
      Seq("location", "do not merge"),
    )
  }


  "TODO" in {
    val pageContent: PageContent = InterpreterSchema.createPageContent(
      s"""
#!Schema Person
name\tAha00a
# it's a comment
startDate\t1982-10-23

endDate\t2099-10-23

location\tSeoul

actor\tActor1\tActor2
character\tCharacter1\tCharacter2
""".trim())

    val parseResult: ParseResult = InterpreterSchema.parse(pageContent)
    parseResult.schemaClass shouldBe "Person"

    val seqSeqField: Seq[Seq[String]] = parseResult.seqSeqField
    println(seqSeqField)
  }

}
