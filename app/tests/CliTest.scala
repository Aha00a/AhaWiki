package tests

object CliTest extends App {
  import com.aha00a.commons.Implicits._
  import com.aha00a.tests.TestUtil

  def run(): Unit = {
    val testUtil = new TestUtil(x => println(x))
    import models.WikiContext
    import testUtil.assertEquals
    val zero = "aa".toIntOrZero
    testUtil.assertEquals(0, 0)
    assertEquals(zero, 0)
    assertEquals("10".toIntOrZero, 10)

    def testEnglishCaseConverter(): Unit = {
      import com.aha00a.commons.utils.EnglishCaseConverter
      assertEquals(EnglishCaseConverter.splitPascalCase("Person"), Seq("Person"))
      assertEquals(EnglishCaseConverter.splitPascalCase("FrontPage"), Seq("Front", "Page"))


      assertEquals(EnglishCaseConverter.camelCase2TitleCase("someWordsAreHere"), "Some Words Are Here")
      assertEquals(EnglishCaseConverter.pascalCase2TitleCase("FrontPage"), "Front Page")
      assertEquals(EnglishCaseConverter.pascalCase2TitleCase("TVSeries"), "TV Series")
    }

    testEnglishCaseConverter()

    implicit val wikiContext: WikiContext = WikiContext("UnitTest")(null, null, null, null, null)

    testMacroBr()
    def testMacroBr()(implicit wikiContext: WikiContext): Unit = {
      import logics.wikis.macros.MacroBr
      val empty = ""
      val dummy = "aaaa"

      assertEquals(MacroBr.toHtmlString(empty), "<br/>")
      assertEquals(MacroBr.toHtmlString(dummy), "<br/>")
      assertEquals(MacroBr.extractLink(empty), Seq())
      assertEquals(MacroBr.extractLink(dummy), Seq())
    }

  }
  run()


}
