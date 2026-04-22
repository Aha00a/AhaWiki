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

  "mergeFields" - {
    "simple" in {
      InterpreterSchema.mergeFields(Seq(
        Seq("name", "Aha00a"),
      )) shouldBe Seq(
        Seq("name", "Aha00a"),
      )
    }
    "complex case" in {
      InterpreterSchema.mergeFields(Seq(
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
      )) shouldBe Seq(
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

  "toJsonLdObject" - {
    "returns JSON-LD object with merged same property as array" in {
      val parseResult = ParseResult(
        "Person",
        Seq(
          Seq("name", "Aha00a"),
          Seq("sameAs", "github.com/aha00a"),
          Seq("sameAs", "https://x.com/aha00a"),
        )
      )

      val json = InterpreterSchema.toJsonLdObject(parseResult).get
      (json \ "@type").as[String] shouldBe "Person"
      (json \ "name").as[String] shouldBe "Aha00a"
      (json \ "sameAs").as[Seq[String]] shouldBe Seq("https://github.com/aha00a", "https://x.com/aha00a")
    }

    "normalizes image/url values and adds page level metadata" in {
      val parseResult = ParseResult(
        "Person",
        Seq(
          Seq("image", "/public/img/profile.png"),
          Seq("sameAs", "//x.com/aha00a"),
          Seq("sameAs", "x.com/aha00a"),
          Seq("sameAs", "x.com/aha00a"),
        )
      )

      val json = InterpreterSchema.toJsonLdObject(
        parseResult = parseResult,
        baseUrl = Some("https://example.com"),
        pageUrl = Some("https://example.com/w/Aha00a"),
        pageName = Some("Aha00a"),
        language = Some("ko-KR")
      ).get

      (json \ "image").as[String] shouldBe "https://example.com/public/img/profile.png"
      (json \ "sameAs").as[Seq[String]] shouldBe Seq("https://x.com/aha00a")
      (json \ "@id").as[String] shouldBe "https://example.com/w/Aha00a"
      (json \ "url").as[String] shouldBe "https://example.com/w/Aha00a"
      (json \ "mainEntityOfPage").as[String] shouldBe "https://example.com/w/Aha00a"
      (json \ "name").as[String] shouldBe "Aha00a"
      (json \ "inLanguage").as[String] shouldBe "ko-KR"
    }

    "converts location-like properties to structured values" in {
      val parseResult = ParseResult(
        "Organization",
        Seq(
          Seq("address", "1600 Amphitheatre Parkway"),
          Seq("location", "Mountain View HQ"),
          Seq("geo", "37.422,-122.084"),
          Seq("foundingLocation", "Seoul"),
        )
      )

      val json = InterpreterSchema.toJsonLdObject(parseResult).get

      (json \ "address" \ "@type").as[String] shouldBe "PostalAddress"
      (json \ "address" \ "streetAddress").as[String] shouldBe "1600 Amphitheatre Parkway"
      (json \ "location" \ "@type").as[String] shouldBe "Place"
      (json \ "location" \ "name").as[String] shouldBe "Mountain View HQ"
      (json \ "geo" \ "@type").as[String] shouldBe "GeoCoordinates"
      (json \ "geo" \ "latitude").as[String] shouldBe "37.422"
      (json \ "geo" \ "longitude").as[String] shouldBe "-122.084"
      (json \ "foundingLocation" \ "@type").as[String] shouldBe "Place"
      (json \ "foundingLocation" \ "name").as[String] shouldBe "Seoul"
    }

    "returns None when schema class is empty" in {
      val parseResult = ParseResult(
        "",
        Seq(Seq("name", "Aha00a"))
      )
      InterpreterSchema.toJsonLdObject(parseResult) shouldBe None
    }
  }

}
