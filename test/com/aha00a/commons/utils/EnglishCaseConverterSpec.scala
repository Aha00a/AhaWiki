package com.aha00a.commons.utils

import org.scalatest.freespec.AnyFreeSpec

class EnglishCaseConverterSpec extends AnyFreeSpec {
  "splitPascalCase" in  {
    assert(EnglishCaseConverter.splitPascalCase("Person") === Seq("Person"))
    assert(EnglishCaseConverter.splitPascalCase("FrontPage") === Seq("Front", "Page"))
  }

  "camelCase2TitleCase" in  {
    assert(EnglishCaseConverter.camelCase2TitleCase("someWordsAreHere") === "Some Words Are Here")
  }

  "pascalCase2TitleCase" in  {
    assert(EnglishCaseConverter.pascalCase2TitleCase("FrontPage") === "Front Page")
    assert(EnglishCaseConverter.pascalCase2TitleCase("TVSeries") === "TV Series")
  }
}
