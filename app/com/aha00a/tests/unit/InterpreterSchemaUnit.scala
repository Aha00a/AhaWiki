package com.aha00a.tests.unit

import com.aha00a.tests.TestUtil
import io.circe.Json
import logics.wikis.interpreters.InterpreterSchema
import logics.wikis.interpreters.InterpreterSchema.ParseResult
import com.aha00a.tests.provider.EmptyContextWikiPage
import com.aha00a.tests.provider.RealContextWikiPage
import logics.wikis.interpreters.InterpreterSchema.ParseResult
import models.ContextWikiPage
import models.PageContent


object InterpreterSchemaUnit {
  def run(testUtil: TestUtil): Unit = {
    import testUtil.assertEquals

    assertEquals(InterpreterSchema.name, "Schema")

    assertEquals(
      InterpreterSchema.expandAddress("서울특별시 마포구 망원동 999-999"),
      Seq(
        Seq("서울특별시"),
        Seq("서울특별시", "마포구"),
        Seq("서울특별시", "마포구", "망원동"),
        Seq("서울특별시", "마포구", "망원동", "999-999"),
      )
    )

    assertEquals(
      InterpreterSchema.mergeFields(Seq(
        Seq("name", "Aha00a"),
      )),
      Seq(
        Seq("name", "Aha00a"),
      )
    )

    assertEquals(
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
      )),
      Seq(
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
    )


    {
      val parseResult = ParseResult(
        "Person",
        Seq(
          Seq("name", "Aha00a"),
          Seq("sameAs", "github.com/aha00a"),
          Seq("sameAs", "https://x.com/aha00a"),
        )
      )

      val json = InterpreterSchema.toJsonLdObject(parseResult).get

      assertEquals((json \ "@type").as[String], ("Person"))
      assertEquals((json \ "name").as[String], ("Aha00a"))
      assertEquals((json \ "sameAs").as[Seq[String]], (Seq("https://github.com/aha00a", "https://x.com/aha00a")))
    }

    {
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

      assertEquals((json \ "image").as[String], ("https://example.com/public/img/profile.png"))
//      assertEquals((json \ "sameAs").as[Seq[String]], (Seq("https://x.com/aha00a")))
      assertEquals((json \ "@id").as[String], ("https://example.com/w/Aha00a"))
      assertEquals((json \ "url").as[String], ("https://example.com/w/Aha00a"))
      assertEquals((json \ "mainEntityOfPage").as[String], ("https://example.com/w/Aha00a"))
      assertEquals((json \ "name").as[String], ("Aha00a"))
      assertEquals((json \ "inLanguage").as[String], ("ko-KR"))
    }

    {
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

      assertEquals((json \ "address" \ "@type").as[String], ("PostalAddress"))
      assertEquals((json \ "address" \ "streetAddress").as[String], ("1600 Amphitheatre Parkway"))
      assertEquals((json \ "location" \ "@type").as[String], ("Place"))
      assertEquals((json \ "location" \ "name").as[String], ("Mountain View HQ"))
      assertEquals((json \ "geo" \ "@type").as[String], ("GeoCoordinates"))
      assertEquals((json \ "geo" \ "latitude").as[String], ("37.422"))
      assertEquals((json \ "geo" \ "longitude").as[String], ("-122.084"))
      assertEquals((json \ "foundingLocation" \ "@type").as[String], ("Place"))
      assertEquals((json \ "foundingLocation" \ "name").as[String], ("Seoul"))
    }

    assertEquals(InterpreterSchema.toJsonLdObject(ParseResult("", Seq(Seq("name", "Aha00a")))), None)
  }
}
