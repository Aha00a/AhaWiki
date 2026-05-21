package com.aha00a.tests.unit

import com.aha00a.tests.TestUtil
import logics.wikis.ExtractConvertInjectInterpreter
import logics.wikis.interpreters.InterpreterWiki
import models.ContextWikiPage

object InterpreterWikiUnit {
  def run(testUtil: TestUtil)(implicit contextWikiPage: ContextWikiPage): Unit = {
    import testUtil.assertEquals

    val html = InterpreterWiki.toHtmlString("= Title = #custom-id .hero .compact")
    assert(html.contains("""<div class="HeadingWrappercustom-id">"""))
    assert(html.contains("""<h1 id="custom-id" class="Headinghero Headingcompact">"""))

    val htmlColumns = InterpreterWiki.toHtmlString("""<Columns count=\"3\" gap=\"16\" minWidth=\"220\">\n 1. a\n 1. b\n 1. c\n</Columns>""")

    val htmlDiv = InterpreterWiki.toHtmlString("""<div id=\"box\" class=\"card\" style=\"color:red\" onclick=\"evil()\">\n 1. [FrontPage]\n</div>""")

    {
      val extractor = new ExtractConvertInjectInterpreter() {
        override def getUniqueKey: String = "EXTRACTED_EXTRACTED_EXTRACTED_EXTRACTED"
      }
      val normal = """ 1. aaa [[[111]]] bbb
                     | 1. ccc""".stripMargin
      val extractedNormal = extractor.extract(normal)

      assertEquals(extractedNormal, """ 1. aaa EXTRACTED_EXTRACTED_EXTRACTED_EXTRACTED bbb
                                     | 1. ccc""".stripMargin)
      assertEquals(extractor.originalLineNumber(1), 1)
      assertEquals(extractor.originalLineNumber(2), 2)
    }

    {
      val extractor = new ExtractConvertInjectInterpreter() {
        override def getUniqueKey: String = "EXTRACTED_EXTRACTED_EXTRACTED_EXTRACTED"
      }
      val abnormal = """ 1. aaa [[[
                       |111
                       |222
                       |333
                       |]]] bbb
                       | 1. ccc""".stripMargin
      val extractedAbnormal = extractor.extract(abnormal)

      assertEquals(extractedAbnormal, """ 1. aaa EXTRACTED_EXTRACTED_EXTRACTED_EXTRACTED bbb
                                       | 1. ccc""".stripMargin)
      assertEquals(extractor.originalLineNumber(1), 1)
      assertEquals(extractor.originalLineNumber(2), 6)
    }
  }
}
